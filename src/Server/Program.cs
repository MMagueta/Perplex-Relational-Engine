using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using Configuration;
using Language;
using Microsoft.FSharp.Collections;

class Program
{
    static void Main(string[] args)
    {

        var config = Builder.loadConfiguration();

        Serilog.ILogger logger;

        if (config.IsOk){
            logger = config.ResultValue.Logger;
        }else {
            throw new Exception(config.ErrorValue);
        }

        // Create a TCP/IP socket
        Socket listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

        // Bind the socket to the local endpoint and listen for incoming connections
        try
        {
            System.Console.WriteLine($"Running at {IPAddress.Any}:4000");
            listener.Bind(new IPEndPoint(IPAddress.Any, 4000));
            listener.Listen(10);

            FSharpMap<string, AST.Entity> schema = ExpressDB.Executor.Main.schema;

            while (true)
            {
                Console.WriteLine("Waiting for a connection...");

                // Accept a connection and create a new socket to handle communication
                Socket handler = listener.Accept();

                byte[] buffer = new byte[1024];
                int bytesReceived = handler.Receive(buffer);
                string request = Encoding.ASCII.GetString(buffer, 0, bytesReceived);
                var ast = ExpressDB.Language.Main.generateAST(request);
                string response = "For now there is no response apart from success.";
                try
                {
                    var result = ExpressDB.Executor.Runner.execute(logger, ast.Value, schema);
                    schema = result.Item2;
                    logger.ForContext("ExecutionType", "Runner").Information($"Finished running '{result.Kind}'");
                }
                catch (Exception e)
                {
                    logger.ForContext("ExecutionType", "Runner").Error(e.Message);
                    response = $"Failed: {e.Message}";
                }
                
                byte[] responseBuffer = Encoding.ASCII.GetBytes(response);
                handler.Send(responseBuffer);

                handler.Shutdown(SocketShutdown.Both);
                handler.Close();
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.ToString());
        }
    }
}