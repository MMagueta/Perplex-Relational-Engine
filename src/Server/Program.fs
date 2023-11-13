module Server

open System.Net
open System.Net.Sockets
open System.Text

open Configuration
open Language

let handle (logger: Serilog.ILogger) schema buffer bytesReceived =
    let request = Encoding.ASCII.GetString(buffer, 0, bytesReceived)
    let ast = PerplexDB.Language.Main.generateAST(request)
    let mutable response = "For now there is no response apart from success."
    try
        let result = PerplexDB.Executor.Runner.execute logger ast.Value schema
        match result with
        | PerplexDB.Executor.Runner.Effect (kind, newSchema) -> 
            logger.ForContext("ExecutionContext", "Server").Information($"Finished running '{kind}'")
            Ok (schema, "Response, but for now there is nothing useful here.")
    with ex ->
        logger.ForContext("ExecutionContext", "Server").Error(ex.Message);
        Error $"Failed: {ex.Message}";

let finishHandler (handler: Socket) (response: string) =
    let responseBuffer = Encoding.ASCII.GetBytes(response)
    handler.Send(responseBuffer) |> ignore
    handler.Shutdown(SocketShutdown.Both)
    handler.Close()

let start () =
    let config = Builder.loadConfiguration()
    match config with
    | Ok config ->
        let logger = config.Logger
        use listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        try
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

            listen (PerplexDB.Executor.Main.schema)

        with ex ->
            logger.ForContext("ExecutionContext", "Runner").Information(ex.Message)
    | Error err ->
        failwith err

[<EntryPoint>]
let main _ =
    start ()
    0