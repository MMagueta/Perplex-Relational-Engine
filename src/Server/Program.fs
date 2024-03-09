module Server

open System.Net
open System.Net.Sockets
open System.Text

open Configuration
open Executor

let schema = Schema.loadFromDisk()

let handle (logger: Serilog.ILogger) schema buffer bytesReceived =
    let request = Encoding.UTF8.GetString(buffer, 0, bytesReceived)
    let ast = PerplexDB.Language.Main.generateAST(request)
    let mutable response = "For now there is no response apart from success."
    // try
    let result = Executor.Runner.execute None logger ast.Value schema
    match result with
    | Executor.Runner.Effect (kind, newSchema) -> 
        logger.ForContext("ExecutionContext", "Server").Information($"Finished running '{kind}'")
        Ok (newSchema, "Response, but for now there is nothing useful here.")
    | Executor.Runner.Projection result ->
        logger.ForContext("ExecutionContext", "Server").Information($"Finished running query '{request}'")
        Ok (schema, System.Text.Json.JsonSerializer.Serialize (result, System.Text.Json.JsonSerializerOptions.Default))
    | Executor.Runner.Update ->
        logger.ForContext("ExecutionContext", "Server").Information($"Finished running query '{request}'")
        Ok (schema, "Updated columns")
    // with ex ->
        // logger.ForContext("ExecutionContext", "Server").Error(ex.Message);
        // Error $"Failed: {ex.Message}";

let finishHandler (handler: Socket) (response: string) =
    printfn "%A" response
    let responseBuffer = Encoding.UTF8.GetBytes(response)
    handler.Send(responseBuffer) |> ignore
    handler.Shutdown(SocketShutdown.Both)
    handler.Close()

let start () =
    let config = Builder.loadConfiguration()
    match config with
    | Ok config ->
        let logger = config.Logger
        use listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        // try
        logger.ForContext("ExecutionContext", "Server").Information($"Running at {IPAddress.Any}:4000")
            
        listener.Bind(new IPEndPoint(IPAddress.Any, 4000))
        listener.Listen(10)

        let rec listen schema =
            logger.ForContext("ExecutionContext", "Runner").Information("Waiting for a connection...")
            
            let handler = listener.Accept()
            let buffer = [| for _ in 1..1024 do 0uy |]
            let bytesReceived = handler.Receive(buffer)

            match handle logger schema buffer bytesReceived with
            | Ok (newSchema, response) ->
                finishHandler handler response
                listen newSchema
            | Error response ->
                finishHandler handler response
                listen schema

        listen schema

        // with ex ->
            // logger.ForContext("ExecutionContext", "Runner").Error(ex.Message)
    | Error err ->
        failwith err

[<EntryPoint>]
let main _ =
    start ()
    0
