namespace ExpressDB.Executor

module Optimizer = failwith "Not implemented"

module Runner =
    open Language.AST

    type Schema = Map<Name, Entity>
    and Name = string

    [<RequireQualifiedAccess>]
    module Schema =
        let TableByteSize ((Table table): Entity) =
            table.Attributes
            |> Map.fold (fun acc _ { Position = _; Type' = value } -> acc + value.ByteSize) 0

    let createRow (relationName: string) (schema: Schema) =
        ExpressDB.Pager.PhysicalStorage.Tuple.serialize schema relationName
        >> function
            | Ok ba -> ba
            | Error ex -> failwith ex.Message

    type ExecutionResult =
        | Effect of Kind: string (* Make this a type later so it's possible to know what generated the effect, like INSERTS *)
        | SchemaUpdate of Schema

    /// This function needs to be replaced by allocating the writes
    /// into another representation, not in a page, which is for reads in my design.
    /// - *RowID* and *Position* can be removed once metadata is added to the file header.
    let createPage relationName bytes rowID size =
        { Entity = relationName
          Content = [| { Content = bytes
                         Position = rowID
                         Size = size
                         State = ExpressDB.Pager.PhysicalStorage.PageState.Filled } |] } : ExpressDB.Pager.PhysicalStorage.Page

    let execute (expression: Expression) (schema: Schema) =
        match expression with
        | Expression.Insert (relationName, fields) ->
            fields
            |> Array.fold (fun acc elem -> Map.add elem.FieldName elem.FieldValue acc) Map.empty
            |> createRow relationName schema
            |> fun content ->
                createPage
                    relationName
                    content
                    0 //Hardcoded rowID for now
                    (Schema.TableByteSize schema.[relationName])
            |> ExpressDB.Pager.PhysicalStorage.Tuple.write
            Effect "INSERT"
        | Expression.CreateRelation (relationName, attributes) ->
            attributes // Assuming *relationName* doesn't exist already
            |> Map.toList
            |> List.mapi (fun i (attributeName, type') -> (attributeName, ({ Position = i; Type' = type' }: Kind.FieldMetadata)))
            |> Map
            |> fun attributes -> Map.add relationName (Entity.Table {Name = relationName; Attributes = attributes}) schema
            |> SchemaUpdate
