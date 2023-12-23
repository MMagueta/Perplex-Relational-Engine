namespace Executor

module Main =
    open Language
    open Runner
                
    let schema = Schema.loadFromDisk()
    
    [<EntryPoint>]
    let main _ =
        let logger = 
            match Configuration.Builder.loadConfiguration() with
            | Ok config -> config.Logger
            | Error err -> failwith err

        let createRelationExpr =
            Expression.CreateRelation ("king", Map.empty |> Map.add "name" (Type.TVariableString 20))

        let insertRowExpr1 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Type.TVariableString 20); FieldValue = Value.VVariableString "Gaiseric"} |])

        let insertRowExpr2 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Type.TVariableString 20); FieldValue = Value.VVariableString "Thunderic"} |])
            
        let insertRowExpr3 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Type.TVariableString 20); FieldValue = Value.VVariableString "Balderic"} |])
        
        let schema = Schema.loadFromDisk()
        logger.ForContext("ExecutionContext", "Runner").Debug(schema.ToString())
        
        // Dirty chain just to test 3 inserts
        if Map.isEmpty schema then
            match Runner.execute logger createRelationExpr schema with
            | Runner.ExecutionResult.Effect ("CREATED RELATION", schema) ->
                //Log.Logger.ForContext("ExecutionContext", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                match Runner.execute logger insertRowExpr1 schema with
                | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                    //Log.Logger.ForContext("ExecutionContext", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                    match Runner.execute logger insertRowExpr2 schema with
                    | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                        //Log.Logger.ForContext("ExecutionContext", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                        match Runner.execute logger insertRowExpr3 schema with
                        | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                            logger.ForContext("ExecutionContext", "Runner").Debug(schema.["king"].ToString())
                            ()
                        | _ -> ()
                    | _ -> ()
                | _ -> ()
            | _ -> ()
        else
            match Runner.execute logger insertRowExpr1 schema with
            | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                //Log.Logger.ForContext("ExecutionContext", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                match Runner.execute logger insertRowExpr2 schema with
                | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                    //Log.Logger.ForContext("ExecutionContext", "Runner").ForContext("Identifier", System.Guid.NewGuid()).Debug(schema.["king"].ToString())
                    match Runner.execute logger insertRowExpr3 schema with
                    | Runner.ExecutionResult.Effect ("INSERT", schema) ->
                        logger.ForContext("ExecutionContext", "Runner").Debug(schema.["king"].ToString())
                        ()
                    | _ -> ()
                | _ -> ()
            | _ -> ()

        0
