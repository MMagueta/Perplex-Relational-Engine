namespace ExpressDB.Executor

open System.Diagnostics
open System.IO
open Serilog

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
                    $"{acc}|{name}|{xml}"
                    ) "" schema
            System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/../schema.xml", text)
        let loadFromDisk () =
            try
                let text = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/../schema.xml")
                let map = Map.ofArray (text.Split("|") |> Array.filter (function "" -> false | _ -> true) |> Array.chunkBySize 2 |> Array.map (fun xs -> (xs.[0], xs.[1])))
                Map.map (fun (k: string) v -> Xml.deserialize<Entity>(v)) map
            with :? System.IO.FileNotFoundException ->
                Map.empty
    let createRow (logger: ILogger) (relationName: string) (schema: Schema) =
        ExpressDB.Pager.PhysicalStorage.Row.serialize logger schema relationName
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

    let execute (logger: ILogger) (expression: Expression) (schema: Schema) =
        match expression with
        | Expression.Insert(relationName, fields) ->
            let (Table tableInfo) = schema.[relationName]

            fields
            |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
            |> createRow logger relationName schema
            |> ExpressDB.Pager.PhysicalStorage.Row.write
                logger
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