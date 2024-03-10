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

    exception TransactionRollback of System.IO.FileStream list

    let _ =
        Log.Logger <-
            LoggerConfiguration()
                .MinimumLevel.Debug()
                .Enrich.FromLogContext()
                .WriteTo.ColoredConsole(outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionContext}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .WriteTo.File(__SOURCE_DIRECTORY__ + "/Logs", outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionContext}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .CreateLogger()

    type ExecutionResult =
        | Effect of
            Kind: string *
            Schema.t (* Make this a type later so it's possible to know what generated the effect, like INSERTS *)
        | Projection of (Map<string, Value.t>*IO.Read.OffsetNumber option) array * Schema.t
        | Update
        | Minus of int
        | Plus of int

    let rec execute (streams: System.IO.FileStream list) (logger: ILogger) (expression: Expression.t) (schema: Schema.t) =
        match expression with
        | Expression.Insert(relationName, fields) ->
            
            let (Entity.Relation (_tableInfo, _)) = schema.[relationName]

            let stream = IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100
            match stream with
            | Ok stream ->
                fields
                |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
                // |> createRow logger relationName schema
                |> IO.Write.Disk.writeFact
                    stream
                    schema
                    relationName
                    None
                    // (Schema.TableByteSize schema.[relationName])
                    // tableInfo.RowCount
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
                Schema.persist(updatedSchema)
    
                Effect("INSERT", updatedSchema)
            | Error _ -> failwith "Failed to acquire lock for writing."
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
                    Projection (result, schema)
                | None -> Projection ([||], schema)
            | None ->
                let stream = match IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100 with Ok stream -> stream | Error _ -> failwith "Failed to acquire lock for reading."
                let search = IO.Read.search stream schema relationName attributesToProject refinement indexBuilder
                match search with
                | Some result ->
                    stream.Dispose()
                    Projection (result, schema)
                | None -> stream.Dispose()
                          Projection ([||], schema)

                
        | Expression.Update(relationName, attributeToUpdate, refinement, constraint) when (Map.tryFind relationName schema).IsSome ->
            match List.tryFind (fun (stream: FileStream) -> stream.Name.Contains relationName) streams with
            | Some stream ->
                let (Minus attributeEvaluation) = execute streams logger attributeToUpdate.FieldValue schema
                let (Projection (refinementEvaluation, schema)) = execute streams logger (Expression.Project (relationName, Expression.ProjectionParameter.All, refinement)) schema
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
                    let (Minus attributeEvaluation) = execute [stream] logger attributeToUpdate.FieldValue schema
                    let (Projection (refinementEvaluation, schema)) = execute [stream] logger (Expression.Project (relationName, Expression.ProjectionParameter.All, refinement)) schema
                    printfn "ATTR: %A" attributeEvaluation
                    printfn "REF: %A" refinementEvaluation
                    
                    IO.Read.update stream schema relationName attributeToUpdate.FieldName refinementEvaluation attributeEvaluation constraint
                    |> ignore
                    stream.Dispose()
                    Update
                | Error _ -> failwithf "Failed to acquire lock for updating relation '%s'" relationName

        | Expression.Minus(left, right) ->
            let leftEval = execute streams logger left schema
            let rightEval = execute streams logger right schema
            printfn "LEFT: %A" leftEval
            printfn "RIGHT: %A" rightEval
            match leftEval, rightEval with
            | Projection (x, _), Projection (y, _) ->
                let (Value.VInteger32 leftVal) = (fst x.[0]).["SUM"]
                let (Value.VInteger32 rightVal) = (fst y.[0]).["SUM"]
                Minus (leftVal - rightVal)
            | Projection (x, _), Minus rightVal ->
                let (Value.VInteger32 leftVal) = (fst x.[0]).["Balance"]
                // For handling sum now, gotta fix the parser
                // if leftVal < 0 then
                Minus (rightVal + leftVal)
                // else Minus (leftVal - rightVal)
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
            let streams =
                List.traverseResultM (fun relationName -> IO.Write.Disk.lockedStream ("/tmp/perplexdb/" + relationName + ".ndf") 100) entities
                |> function Ok streams -> streams
                          | Error err -> raise (System.Exception err)

            let walStreams = Async.Parallel (List.map (fun (s: FileStream) -> async {
                                                         let stream = new IO.FileStream(s.Name + ".wal", IO.FileMode.OpenOrCreate, IO.FileAccess.Write)
                                                         do! s.CopyToAsync(stream) |> Async.AwaitTask
                                                         return stream }) streams)
                             |> Async.RunSynchronously
                             |> List.ofArray
            try
                let reducer acc cmd =
                    match cmd with
                    | Expression.LockRead _ ->
                        logger.Warning "Lock for reads not considered in blocks as of now. WIP"
                        acc
                    | Expression.Update (_, _, _, _) ->
                        let _ =
                            execute walStreams logger cmd schema
                        acc
                    | Expression.Insert (_, _) ->
                        let result =
                            execute walStreams logger cmd schema
                        match result with
                        | ExecutionResult.Effect(_, newSchema) -> schema <- newSchema
                        | _ -> ()
                        acc
                    | Expression.Project (_, _, _) ->
                        let result =
                            execute walStreams logger cmd schema
                        match result with
                        | ExecutionResult.Projection _ as projection -> Some projection
                        | _ -> acc
                    | Expression.LockWrite _ ->
                        logger.Warning "Lock for writes is currently only available for updates and inserts. WIP"
                        acc
                    | otherwise -> logger.Error ("Did not expect '{@Otherwise}' in a transact block.", otherwise)
                                   acc
    
                let zip = List.zip walStreams streams
    
                Async.Parallel (List.map (fun (w: FileStream, s: FileStream) ->
                                            async { return! w.CopyToAsync(s) |> Async.AwaitTask }) zip)
                |> Async.RunSynchronously
                |> ignore

                let result = List.fold reducer None commands
                List.iter (fun (w: System.IO.FileStream, s: System.IO.FileStream) -> s.Dispose(); w.Dispose()) zip
                
                Option.defaultValue (Effect("TRANSACTION BLOCK EXECUTED", schema)) result

            with _ -> 
                List.iter (fun (s: System.IO.FileStream) -> s.Dispose()) streams
                
                raise <| TransactionRollback walStreams

        | otherwise -> failwithf "NOT EXPECTING %A" otherwise
            
