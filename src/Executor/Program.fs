namespace Executor

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

    let execute (logger: ILogger) (expression: Expression.t) (schema: Schema.t) =
        match expression with
        | Expression.Insert(relationName, fields) ->
            let (Entity.Relation (tableInfo, _)) = schema.[relationName]

            fields
            |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
            // |> createRow logger relationName schema
            |> IO.Write.Disk.writeFact
                schema
                relationName
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
            
            Schema.persist(updatedSchema)

            Effect("INSERT", updatedSchema)
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
        | Expression.CreateRelation(relationName, _) when (Map.tryFind relationName schema).IsSome ->
            let msg = "Attempted to recreate relation without specifying explicit overwrite. Try `CREATE RELATION OVERRIDE`"
            Log.Logger.ForContext("ExecutionContext", "Serialization").ForContext("Identifier", System.Guid.NewGuid()).Error(msg)
            failwith msg
