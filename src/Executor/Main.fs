namespace ExpressDB.Executor

open ExpressDB.Executor.Runner

module Main =
    open Language.AST
    open Serilog

    let _ =
        Log.Logger <-
            LoggerConfiguration()
                .MinimumLevel.Debug()
                .Enrich.FromLogContext()
                .WriteTo.ColoredConsole(outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionType}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .WriteTo.File(__SOURCE_DIRECTORY__ + "/Logs", outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionType}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .CreateLogger()
    
    [<EntryPoint>]
    let main _ =
        let createRelationExpr =
            Expression.CreateRelation ("king", Map.empty |> Map.add "name" (Types.VariableCharacters 20))

        let insertRowExpr1 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Types.VariableCharacters 20); FieldValue = ELiteral.LVarChar "Gaiseric"} |])

        let insertRowExpr2 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Types.VariableCharacters 20); FieldValue = ELiteral.LVarChar "Thunderic"} |])
            
        let insertRowExpr3 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Types.VariableCharacters 20); FieldValue = ELiteral.LVarChar "Balderic"} |])
        
        let schema = Schema.loadFromDisk()
        Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.ToString())
        
        // Dirty chain just to test 3 inserts
        if Map.isEmpty schema then
            match Runner.execute createRelationExpr schema with
            | Runner.ExecutionResult.Effect ("CREATED RELATION", schema) ->
                //Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                match Runner.execute insertRowExpr1 schema with
                | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                    //Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                    match Runner.execute insertRowExpr2 schema with
                    | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                        //Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                        match Runner.execute insertRowExpr3 schema with
                        | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                            Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                            ()
                        | _ -> ()
                    | _ -> ()
                | _ -> ()
            | _ -> ()
        else
            match Runner.execute insertRowExpr1 schema with
            | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                //Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                match Runner.execute insertRowExpr2 schema with
                | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                    //Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                    match Runner.execute insertRowExpr3 schema with
                    | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                        Log.Logger.ForContext("ExecutionType", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                        ()
                    | _ -> ()
                | _ -> ()
            | _ -> ()

        0
