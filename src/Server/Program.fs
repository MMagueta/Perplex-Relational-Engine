module Server

open System.Net
open System.Net.Sockets
open System.Text

open System.Threading

open Configuration
open Executor

let mutable schema =
    Schema.loadFromDisk()

let handle (logger: Serilog.ILogger) schema ast request =
    // try
    let result = Executor.Runner.execute [] logger ast schema
    match result with
    | Executor.Runner.Effect (kind, newSchema) -> 
        logger.ForContext("ExecutionContext", "Server").Information($"Finished running '{kind}'")
        Ok (newSchema, FSharp.Json.Json.serialize {|Message = "Effects performed successfully."|})
    | Executor.Runner.Projection (result, _) ->
        logger.ForContext("ExecutionContext", "Server").Information($"Finished running query '{request}'")
        Ok (schema, FSharp.Json.Json.serialize (Array.map fst result))
    | Executor.Runner.Update ->
        logger.ForContext("ExecutionContext", "Server").Information($"Finished running query '{request}'")
        Ok (schema, FSharp.Json.Json.serialize {|Message = "Updates performed successfully."|})
    
    | otherwise ->
        logger.ForContext("ExecutionContext", "Server").Error("Failed on '{@Request}': {@Otherwise}", request, otherwise)
        Error $"Engine unexpected return: '{otherwise}'"
    // with ex ->
        // logger.ForContext("ExecutionContext", "Server").Error(ex.Message);
        // Error $"Failed: {ex.Message}";

let finishHandler (handler: Socket) (response: string) =
    printfn "%A" response
    let responseBuffer = Encoding.UTF8.GetBytes(response)
    handler.Send(responseBuffer) |> ignore
    handler.Shutdown(SocketShutdown.Both)
    handler.Close()

let rec performer logger (handler: Socket) = async {
    try
        let buffer = [| for _ in 1..1024 do 0uy |]
        let bytesReceived = handler.Receive(buffer)
        let request = Encoding.UTF8.GetString(buffer, 0, bytesReceived)
        match PerplexDB.Language.Main.generateAST(request) with
        | Some (Language.Expression.LockWrite ast) -> 
            let rwl = new ReaderWriterLockSlim()
            if rwl.TryEnterWriteLock 100 then
                match handle logger schema ast request with
                | Ok (newSchema, response) ->
                    finishHandler handler response
                    try schema <- newSchema
                    finally rwl.ExitWriteLock(); rwl.Dispose()
                | Error response ->
                    finishHandler handler response
            else finishHandler handler "Failed to acquire write lock."
        | Some (Language.Expression.LockRead ast) ->
            let rwl = new ReaderWriterLockSlim()
            if rwl.TryEnterReadLock 100 then
                match handle logger schema ast request with
                | Ok (newSchema, response) ->
                    finishHandler handler response
                    try schema <- newSchema
                    finally rwl.ExitReadLock(); rwl.Dispose()
                | Error response ->
                    finishHandler handler response
            else finishHandler handler "Failed to acquire read lock."
        | Some ((Language.Expression.Begin _) as block) ->
            let rwl = new ReaderWriterLockSlim()
            if rwl.TryEnterWriteLock 100 then
                match handle logger schema block request with
                | Ok (newSchema, response) ->
                    finishHandler handler response
                    try schema <- newSchema
                    finally rwl.ExitWriteLock(); rwl.Dispose()
                | Error response ->
                    finishHandler handler response
            else finishHandler handler "Failed to acquire write lock."
        | Some _ -> finishHandler handler "Requires to acquire a lock. Please use LOCK (READ|WRITE) before the query expression."
        | None -> finishHandler handler "Nothing to do."
    with
    | :? IO.Read.ViolationOfConstraint as ex -> finishHandler handler (FSharp.Json.Json.serialize {|ErrorCode = 1; Message = ex.Data0|})
    | :? Executor.Runner.TransactionRollback as ex ->
        // Catching since it might be useful
        ex.Data0
        |> List.iter (fun w -> w.Dispose())
        finishHandler handler (FSharp.Json.Json.serialize {|ErrorCode = 2; Message = "Rolling back transaction."|})
    | ex -> logger.ForContext("ExecutionContext", "Runner").Error("{@Error}: {@Stacktrace}", ex.Message, ex.StackTrace)
    return ()
}

let start () =
    let config = Builder.loadConfiguration()
    match config with
    | Ok config ->
        let logger = config.Logger
        use listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        logger.ForContext("ExecutionContext", "Server").Information($"Running at {IPAddress.Any}:4000")
            
        listener.Bind(new IPEndPoint(IPAddress.Any, 4000))
        listener.Listen(10)

        let rec listen () =
            try
                logger.ForContext("ExecutionContext", "Runner").Information("Waiting for a connection...")
                
                let handler = listener.Accept()
                Async.Start(performer logger handler)
            with ex ->
                logger.ForContext("ExecutionContext", "Runner").ForContext("StackTrace", ex.StackTrace).Error("{@Error}: {@Stacktrace}", ex.Message, ex.StackTrace)
            listen ()
        listen ()

        
    | Error err ->
        failwith err

[<EntryPoint>]
let main _ =
    start ()
    0
