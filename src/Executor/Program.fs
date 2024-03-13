namespace Executor

open System
open System.IO
open Serilog

module private Xml =
    open System.Runtime.Serialization
    open System.Text

    let serialize<'T> (myObj: 'T) =   
        use ms = new MemoryStream() 
        DataContractSerializer(typeof<'T>).WriteObject(ms, myObj) 
        Encoding.Default.GetString(ms.ToArray())

    let deserialize<'T> (jsonString: string) =  
        use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(jsonString)) 
        let obj = DataContractSerializer(typeof<'T>).ReadObject(ms) 
        obj :?> 'T
        
[<RequireQualifiedAccess>]
module Schema = begin
    let persist (schema: Language.Schema.t) =
        let text =
            Map.fold (fun acc name entity ->
                let xml = Xml.serialize<Language.Entity.t> entity
                $"{acc}|{name}|{xml}"
                ) "" schema
        File.WriteAllText("/tmp/perplexdb/schema.xml", text)
    let loadFromDisk () =
        try
            let text = File.ReadAllText("/tmp/perplexdb/schema.xml")
            let map = Map.ofArray (text.Split("|") |> Array.filter (function "" -> false | _ -> true) |> Array.chunkBySize 2 |> Array.map (fun xs -> (xs.[0], xs.[1])))
            Map.map (fun _ v -> Xml.deserialize<Language.Entity.t>(v)) map
        with :? FileNotFoundException ->
            Map.empty
end

module Runner =
    open Language

    open FsToolkit.ErrorHandling

    exception TransactionRollback of Reason: string

    let _ =
        Log.Logger <-
            LoggerConfiguration()
                .MinimumLevel.Debug()
                .Enrich.FromLogContext()
                .WriteTo.ColoredConsole(outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionContext}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .WriteTo.File(__SOURCE_DIRECTORY__ + "/Logs", outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionContext}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .CreateLogger()

    type ExecutionResult =
        | Insert of NewSchema: Schema.t * IO.Write.Fact * RelationName: string
        | Effect of
            Kind: string *
            Schema.t (* Make this a type later so it's possible to know what generated the effect, like INSERTS *)
        | Projection of (Map<string, Value.t>*IO.Read.OffsetNumber option) array * Schema.t * RelationName: string
        | Update
        | Minus of int
        | Plus of int

    let rec execute (carry: (IO.Write.Fact * string) option) commit (streams: FileStream list) (logger: ILogger) (expression: Expression.t) (schema: Schema.t) =
        match expression with
        | Expression.Insert(relationName, fields) ->
            let (Entity.Relation (_tableInfo, _)) = schema.[relationName]
            match List.tryFind (fun (stream: FileStream) -> stream.Name.Contains relationName) streams with
            | Some stream ->
                let fact: IO.Write.Fact = fields |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
                if commit then
                    fact
                    |> IO.Write.Disk.writeFact
                        stream
                        schema
                        relationName
                        None
                    let updatedSchema =
                        Map.change
                            relationName
                            (function
                            | (Some(Entity.Relation (relationAttributes, physicalOffset))) ->
                                Entity.Relation (relationAttributes, physicalOffset + 1l)
                                |> Some
                            | _ -> None)
                            schema
                    Schema.persist(updatedSchema)
        
                    Insert(updatedSchema, fact, relationName)
                else
                    let updatedSchema =
                        Map.change
                            relationName
                            (function
                            | (Some(Entity.Relation (relationAttributes, physicalOffset))) ->
                                Entity.Relation (relationAttributes, physicalOffset + 1l)
                                |> Some
                            | _ -> None)
                            schema
                    Insert(updatedSchema, fact, relationName)
            | None ->
                let (Ok stream) = IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100
                let fact: IO.Write.Fact = fields |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
                if commit then
                    IO.Write.Disk.writeFact
                        stream
                        schema
                        relationName
                        None
                        fact
                    let updatedSchema =
                        Map.change
                            relationName
                            (function
                            | (Some(Entity.Relation (relationAttributes, physicalOffset))) ->
                                Entity.Relation (relationAttributes, physicalOffset + 1l)
                                |> Some
                            | _ -> None)
                            schema
                    Schema.persist(updatedSchema)
                    stream.Dispose()
                    Insert(updatedSchema, fact, relationName)
                else
                    let updatedSchema =
                        Map.change
                            relationName
                            (function
                            | (Some(Entity.Relation (relationAttributes, physicalOffset))) ->
                                Entity.Relation (relationAttributes, physicalOffset + 1l)
                                |> Some
                            | _ -> None)
                            schema
                    stream.Dispose()
                    Insert(updatedSchema, fact, relationName)

        | Expression.CreateRelation(relationName, attributes) when (Map.tryFind relationName schema).IsNone ->
            let updatedSchema =
                attributes // Assuming *relationName* doesn't exist already
                |> Map.toList
                |> List.mapi (fun i (attributeName, type') ->
                    (attributeName, ({ fieldPosition = i; type' = type' }: Entity.FieldMetadata)))
                |> Map
                |> fun attributes ->
                    Map.add
                        relationName
                        (Entity.Relation (attributes, 0l))
                        schema
                        
            Schema.persist(updatedSchema)
            Effect("CREATED RELATION", updatedSchema)
            
        | Expression.Project(relationName, attributesToProject, refinement) when (Map.tryFind relationName schema).IsSome ->
            // let search = IO.Read.search schema relationName (Language.Expression.ProjectionParameter.Restrict attributesToProject) ("Age", 23)
            let indexBuilder: IO.Read.IndexBuilder =
                fun offset chunkNumber pageNumber instanceNumber columns ->
                    match Map.tryFind "AccountNumber" columns with
                    | Some (Value.VInteger32 v) ->
                        { entity = columns
                          key = v
                          chunkNumber = chunkNumber
                          offset = offset
                          pageNumber = pageNumber
                          slotNumber = instanceNumber }
                    | _ -> failwith "AAA"
            // let stream =
                // stream
                // |> Option.map Ok
                // |> Option.defaultValue (IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100)
            match List.tryFind (fun (stream: FileStream) -> stream.Name.Contains relationName) streams with
            | Some stream ->
                let search = IO.Read.search stream schema relationName attributesToProject refinement indexBuilder
                match search with
                | Some result ->
                    Projection (result, schema, relationName)
                | None -> Projection ([||], schema, relationName)
            | None ->
                let stream = match IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100 with Ok stream -> stream | Error _ -> failwith "Failed to acquire lock for reading."
                let search = IO.Read.search stream schema relationName attributesToProject refinement indexBuilder
                match search with
                | Some result ->
                    stream.Dispose()
                    Projection (result, schema, relationName)
                | None -> stream.Dispose()
                          Projection ([||], schema, relationName)

                
        | Expression.Update(relationName, attributeToUpdate, refinement, constraint) when (Map.tryFind relationName schema).IsSome ->
            match List.tryFind (fun (stream: FileStream) -> stream.Name.Contains relationName) streams with
            | Some stream ->
                let (Minus attributeEvaluation) = execute carry commit streams logger attributeToUpdate.FieldValue schema
                let (Projection (refinementEvaluation, schema, _relationName)) = execute carry commit streams logger (Expression.Project (relationName, Expression.ProjectionParameter.All, refinement)) schema
                printfn "ATTR: %A" attributeEvaluation
                printfn "REF: %A" refinementEvaluation
                // BUG: Stream is closing on updates, but it should not be SOME since the start
                if stream.CanWrite then
                    IO.Read.update stream schema relationName attributeToUpdate.FieldName refinementEvaluation attributeEvaluation constraint
                    |> ignore
                else 
                    stream.Dispose()
                    let (Ok stream) = IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100
                    IO.Read.update stream schema relationName attributeToUpdate.FieldName refinementEvaluation attributeEvaluation constraint
                    |> ignore
                    stream.Dispose()
                Update
            | None ->
                let stream = IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100
                match stream with
                | Ok stream ->
                    let (Minus attributeEvaluation) = execute carry commit [stream] logger attributeToUpdate.FieldValue schema
                    let (Projection (refinementEvaluation, schema, _relationName)) = execute carry commit [stream] logger (Expression.Project (relationName, Expression.ProjectionParameter.All, refinement)) schema
                    printfn "ATTR: %A" attributeEvaluation
                    printfn "REF: %A" refinementEvaluation
                    
                    IO.Read.update stream schema relationName attributeToUpdate.FieldName refinementEvaluation attributeEvaluation constraint
                    |> ignore
                    stream.Dispose()
                    Update
                | Error _ -> failwithf "Failed to acquire lock for updating relation '%s'" relationName

        | Expression.Minus(left, right) ->
            match carry with
            | Some (map, relationName) when not <| Map.isEmpty map ->
                let leftEval = execute carry commit streams logger left schema
                let rightEval = execute carry commit streams logger right schema
                printfn "LEFT: %A" leftEval
                printfn "RIGHT: %A" rightEval
                match leftEval, rightEval with
                | Projection (x, _, relationNameX), Projection (y, _, relationNameY) ->
                    let (Value.VInteger32 leftVal) = (fst x.[0]).["SUM"]
                    let (Value.VInteger32 rightVal) = (fst y.[0]).["SUM"]
                    if relationName = relationNameX || relationName = relationNameY then
                        let remainderFromTransaction: int =
                            Map.fold (fun acc k v ->
                                match v with
                                | Value.VInteger32 i ->
                                    if k = "Value" && relationName = "Credit" then acc + i elif k = "Value" && relationName = "Debit" then acc - i else acc
                                | Value.VVariableString _ -> acc) 0 map
                        Minus (leftVal - rightVal + remainderFromTransaction)
                    else
                        Minus (leftVal - rightVal)
                | Projection (x, _, relationName), Minus rightVal ->
                    let (Value.VInteger32 leftVal) = (fst x.[0]).["InitialBalance"]
                    // For handling sum now, gotta fix the parser
                    Minus (leftVal + rightVal)
                | _ -> failwith ""
            | _ ->
                let leftEval = execute carry commit streams logger left schema
                let rightEval = execute carry commit streams logger right schema
                printfn "LEFT: %A" leftEval
                printfn "RIGHT: %A" rightEval
                match leftEval, rightEval with
                | Projection (x, _, _), Projection (y, _, _) ->
                    let (Value.VInteger32 leftVal) = (fst x.[0]).["SUM"]
                    let (Value.VInteger32 rightVal) = (fst y.[0]).["SUM"]
                    Minus (leftVal - rightVal)
                | Projection (x, _, _), Minus rightVal ->
                    let (Value.VInteger32 leftVal) = (fst x.[0]).["Balance"]
                    // For handling sum now, gotta fix the parser
                    Minus (leftVal + rightVal)
                | _ -> failwith ""

        (*
        | Expression.Plus(left, right) ->
            let leftEval = execute stream logger left schema
            let rightEval = execute stream logger right schema
            printfn "LEFT: %A" leftEval
            printfn "RIGHT: %A" rightEval
            match leftEval, rightEval with
            | Projection x, Minus rightVal ->
                let (Value.VInteger32 leftVal) = (fst x.[0]).["Balance"]
                Plus (leftVal + rightVal)
            | _ -> failwith ""
        *)
        
        | Expression.Begin (entities, commands) ->
            let mutable schema = schema
            let mutable carry = None
            let streams =
                List.traverseResultM (fun relationName -> IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100) entities
                |> function Ok streams -> streams
                          | Error err -> raise (System.Exception err)

            try
                let reducer acc cmd =
                    match cmd with
                    | Expression.LockRead _ ->
                        logger.Warning "Lock for reads not considered in blocks as of now. WIP"
                        acc
                    | Expression.Update (_, _, _, _) ->
                        let _ =
                            execute carry commit streams logger cmd schema
                        acc
                    | Expression.Insert (_, _) ->
                        let result =
                            execute carry false streams logger cmd schema
                        match result with
                        | ExecutionResult.Insert(newSchema, stringMap, relationInserted) ->
                            carry <- Some (stringMap, relationInserted)
                            //schema <- newSchema
                        | _ -> ()
                        acc
                    | Expression.Project (_, _, _) ->
                        let result =
                            execute carry commit streams logger cmd schema
                        match result with
                        | ExecutionResult.Projection _ as projection -> Some projection
                        | _ -> acc
                    | Expression.LockWrite _ ->
                        logger.Warning "Lock for writes is currently only available for updates and inserts. WIP"
                        acc
                    | otherwise -> logger.Error ("Did not expect '{@Otherwise}' in a transact block.", otherwise)
                                   acc
                
                match List.fold reducer None commands, carry with
                | Some result, Some (fact, relationName) ->
                    let stream = List.find (fun (s: FileStream) -> s.Name.Contains relationName) streams
                    IO.Write.Disk.writeFact
                        stream
                        schema
                        relationName
                        None
                        fact
                    let updatedSchema =
                        Map.change
                            relationName
                            (function
                            | (Some(Entity.Relation (relationAttributes, physicalOffset))) ->
                                Entity.Relation (relationAttributes, physicalOffset + 1l)
                                |> Some
                            | _ -> None)
                            schema
                    Schema.persist(updatedSchema)
                    
                    List.iter (fun (s: FileStream) -> s.Dispose()) streams
                    
                    result
                | _ ->
                    
                    List.iter (fun (s: FileStream) -> s.Dispose()) streams
                    Effect("FINISHED TRANSACTION BLOCK", schema)
                    
            with ex -> //Rollback
                List.iter (fun (s: FileStream) -> s.Dispose()) streams
                
                logger.ForContext("ExecutionContext", "Executor").Fatal("Rolling back transaction due to: {@Error}", ex.Message)
                
                let reason = ex.Message
                
                raise <| TransactionRollback reason

        | otherwise -> failwithf "NOT EXPECTING %A" otherwise
            
