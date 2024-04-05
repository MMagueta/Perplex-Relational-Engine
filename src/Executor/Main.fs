namespace Executor

open System.IO

module Main =
    open Language
    open Runner
                
    let schema = Schema.loadFromDisk()
    (*
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
    *)
    [<EntryPoint>]
    let testPager _ =
        let indexBuilder: IO.Read.IndexBuilder =
            fun offset chunkNumber pageNumber instanceNumber columns ->
                match Map.tryFind "id" columns with
                | Some (Value.VInteger32 v) ->
                    { entity = columns
                      key = v
                      chunkNumber = chunkNumber
                      offset = offset
                      pageNumber = pageNumber
                      slotNumber = instanceNumber }
                | _ -> failwith "AAA"
                
        let logger = 
            match Configuration.Builder.loadConfiguration() with
            | Ok config -> config.Logger
            | Error err -> failwith err
        
        use stream = new System.IO.FileStream ("/tmp/perplexdb/king.ndf", FileMode.OpenOrCreate, FileAccess.ReadWrite)
        
        let createRelationExpr =
            Expression.CreateRelation ("king", Map.empty |> Map.add "name" (Type.TVariableString 20) |> Map.add "id" Type.TInteger32)
            
        let insertRowExpr1 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Type.TVariableString 20); FieldValue = Value.VVariableString "Gaiseric"}
                                          {FieldName = "id"; FieldType = Type.TInteger32; FieldValue = Value.VInteger32 1} |])
            
        let insertRowExpr2 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Type.TVariableString 20); FieldValue = Value.VVariableString "Huneric"}
                                          {FieldName = "id"; FieldType = Type.TInteger32; FieldValue = Value.VInteger32 1} |])
            
        let insertRowExpr3 =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Type.TVariableString 20); FieldValue = Value.VVariableString "Alberic"}
                                          {FieldName = "id"; FieldType = Type.TInteger32; FieldValue = Value.VInteger32 1} |])
        let mutable schema = schema
        Runner.execute None true [stream] logger createRelationExpr schema
        |> function Effect("CREATED RELATION", updatedSchema) -> schema <- updatedSchema
        Runner.execute None true [stream] logger insertRowExpr1 schema
        |> function Insert(updatedSchema,_,_) -> schema <- updatedSchema
        Runner.execute None true [stream] logger insertRowExpr2 schema
        |> function Insert(updatedSchema,_,_) -> schema <- updatedSchema
        Runner.execute None true [stream] logger insertRowExpr3 schema
        |> function Insert(updatedSchema,_,_) -> schema <- updatedSchema
        
        IO.Read.search stream schema "king" (Language.Expression.ProjectionParameter.Restrict ["id"]) None indexBuilder
        |> printfn "RESULT: %A"
        
        System.IO.File.Delete ("/tmp/perplexdb/king.ndf")
        System.IO.File.Delete ("/tmp/perplexdb/schema.xml")
        0
