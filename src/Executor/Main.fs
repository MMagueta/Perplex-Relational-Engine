namespace Executor

module Main =
    open Language
    open Runner
                
    let schema = Schema.loadFromDisk()
        
    let testEffects _ =
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

    [<EntryPoint>]
    let testPager _ =
        let indexBuilder: IO.Read.IndexBuilder =
            fun chunkNumber pageNumber instanceNumber columns ->
                match Map.tryFind "Age" columns with
                | Some (Value.VInteger32 v) ->
                    { entity = columns
                      key = v
                      chunkNumber = chunkNumber
                      pageNumber = pageNumber
                      slotNumber = instanceNumber }
                | _ -> failwith ""

        IO.Read.search schema "Person" (Language.Expression.ProjectionParameter.Restrict ["Name"]) (Some 25) indexBuilder
        |> printfn "%A"
        0
