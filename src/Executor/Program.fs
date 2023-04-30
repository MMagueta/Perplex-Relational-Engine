namespace ExpressDB.Executor

open System
open System.Diagnostics
open System.IO
open Serilog
open System.IO.Compression

module Xml =
    open System.IO
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
        
module Optimizer =
    failwith "Not implemented"

module Runner =
    open Language.AST

    let _ =
        Log.Logger <-
            LoggerConfiguration()
                .MinimumLevel.Debug()
                .Enrich.FromLogContext()
                .WriteTo.ColoredConsole(outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionType}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .WriteTo.File(__SOURCE_DIRECTORY__ + "/Logs", outputTemplate = "{Timestamp:yyyy-MM-dd HH:mm:ss} [{Level}] [{ExecutionType}-{Identifier}] {Message:l}{NewLine}{Exception}")
                .CreateLogger()
    
    type Schema = Map<Name, Entity>
    and Name = string
    
    [<RequireQualifiedAccess>]
    module Schema =
        let TableByteSize ((Table table): Entity) =
            table.Attributes
            |> Map.fold (fun acc _ { Position = _; Type' = value } -> acc + value.ByteSize) 0
            
        let persist (schema: Schema) =
            let text =
                Map.fold (fun acc name entity ->
                          let xml = Xml.serialize<Entity> entity
                          $"{acc}|{name}|{xml}") "" schema
            
            // Switch this to FsConfig later
            if Environment.GetEnvironmentVariable "SCHEMA_COMPRESSION" = "OFF" then
                System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/../schema.xs", text)
            else
                use memoryStream = new MemoryStream()
                use gzipStream = new GZipStream(memoryStream, CompressionLevel.Optimal)
                let byteStream: byte array =
                    text
                    |> Array.ofSeq
                    |> Array.collect (System.BitConverter.GetBytes >> Array.take 1)
                gzipStream.Write(byteStream, 0, byteStream.Length);
                let result = memoryStream.ToArray()
                System.IO.File.WriteAllBytes(__SOURCE_DIRECTORY__ + "/../schema.xs", result)
            
            
        let loadFromDisk () =
            if Environment.GetEnvironmentVariable "SCHEMA_COMPRESS" = "OFF" then
                try
                    let text = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/../schema.xs")
                    let map =
                        Map.ofArray
                          (text.Split("|")
                           |> Array.filter (function "" -> false | _ -> true)
                           |> Array.chunkBySize 2
                           |> Array.map (fun xs -> (xs.[0], xs.[1])))
                    Map.map (fun _ v -> Xml.deserialize<Entity>(v)) map
                with :? System.IO.FileNotFoundException ->
                    Map.empty
                   | :? System.IndexOutOfRangeException | :? System.IO.InvalidDataException as ex ->
                       if ex.Message = "The archive entry was compressed using an unsupported compression method." || ex.Message = "Index was outside the bounds of the array."
                       then Log.Logger.ForContext("ExecutionType", "LoadingSchema").ForContext("Identifier", System.Guid.NewGuid()).Error("The schema is compressed. Consider changing the environment variable `SCHEMA_COMPRESS` to `ON`")
                            failwith "The schema is compressed. Consider changing the environment variable `SCHEMA_COMPRESS` to `ON`"
                       else let msg = "The schema data seems to be corrupted. If it is not GZip compressed and it was previously saved as so, consider going back to GZip; unless the data was uncarefully modified."
                            Log.Logger.ForContext("ExecutionType", "LoadingSchema").ForContext("Identifier", System.Guid.NewGuid()).Error(msg)
                            failwith msg
            else
                try
                    let bytes = System.IO.File.ReadAllBytes(__SOURCE_DIRECTORY__ + "/../schema.xs")
                    use memoryStream = new MemoryStream(bytes)
                    use outStream = new MemoryStream()
                    use decompressStream = new GZipStream(memoryStream, CompressionMode.Decompress)
                    decompressStream.CopyTo(outStream)
                    let map =
                        Map.ofArray
                          (System.Text.Encoding.UTF8.GetString(outStream.ToArray()).Split("|")
                           |> Array.filter (function "" -> false | _ -> true)
                           |> Array.chunkBySize 2
                           |> Array.map (fun xs -> (xs.[0], xs.[1])))
                    Map.map (fun _ v -> Xml.deserialize<Entity>(v)) map
                with :? System.IO.FileNotFoundException ->
                    Map.empty
                    | :? System.IO.InvalidDataException -> printfn "HELP"; Map.empty
                
    let createRow (relationName: string) (schema: Schema) =
        ExpressDB.Pager.PhysicalStorage.Tuple.serialize schema relationName
        >> function
            | Ok ba -> ba
            | Error ex -> failwith ex.Message

    type ExecutionResult =
        | Effect of
            Kind: string *
            Schema (* Make this a type later so it's possible to know what generated the effect, like INSERTS *)

    /// This function needs to be replaced by allocating the writes
    /// into another representation, not in a page, which is for reads in my design.
    /// - *RowID* and *Position* can be removed once metadata is added to the file header.
    let createPage relationName bytes rowID size =
        { Entity = relationName
          Header =
            { StartingPosition = 0
              EndingPosition = 1 }
          Content =
            [| { Content = bytes
                 Position = rowID
                 Size = size
                 State = ExpressDB.Pager.PhysicalStorage.PageState.Filled } |] }
        : ExpressDB.Pager.PhysicalStorage.Page

    let execute (expression: Expression) (schema: Schema) =
        match expression with
        | Expression.Insert(relationName, fields) ->
            let (Table tableInfo) = schema.[relationName]

            fields
            |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
            |> createRow relationName schema
            |> ExpressDB.Pager.PhysicalStorage.Tuple.write
                tableInfo.RowCount
                (Schema.TableByteSize schema.[relationName])
                relationName
            let updatedSchema =
                Map.change
                    relationName
                    (function
                    | (Some(Table tableInfo)) ->
                        Table
                            { tableInfo with
                                RowCount = tableInfo.RowCount + 1 }
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
                    (attributeName, ({ Position = i; Type' = type' }: Kind.FieldMetadata)))
                |> Map
                |> fun attributes ->
                    Map.add
                        relationName
                        (Entity.Table
                            { Name = relationName
                              RowCount = 0
                              Attributes = attributes })
                        schema
                        
            Schema.persist(schema)

            Effect("CREATED RELATION", updatedSchema)
        | Expression.CreateRelation(relationName, _) when (Map.tryFind relationName schema).IsSome ->
            let msg = "Attempted to recreate relation without specifying explicit overwrite. Try `CREATE RELATION OVERRIDE`"
            Log.Logger.ForContext("ExecutionType", "Serialization").ForContext("Identifier", System.Guid.NewGuid()).Error(msg)
            failwith msg
