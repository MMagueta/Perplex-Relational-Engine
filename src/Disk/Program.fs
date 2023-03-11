module ExpressDB.Pager.PhysicalStorage

open System
open System.IO
open Language
open Language.AST

type Schema = Map<Name, Entity>
and Name = string

[<RequireQualifiedAccess>]
module Schema =
    let TableByteSize ((Table table): Entity) =
        table.Attributes
        |> Map.fold (fun acc k { Position = _; Type' = value } -> acc + value.ByteSize) 0

type PageState =
    | Filled
    | Empty

type PageSlot =
    { Content: byte array
      State: PageState
      Size: int
      Position: int }

type Page =
    { Content: PageSlot array
      Entity: Name }

[<RequireQualifiedAccessAttribute>]
module ExpressDBError =
    exception Serialization of string

module Tuple =
    let blanks lowerBound upperBound =
        seq {
            for _ in lowerBound..upperBound do
                0uy
        }

    let serialize (schema: Schema) (entity: Name) (tuple: Map<Name, ELiteral>) : Result<byte array, exn> =
        let convert (metadata: Kind.Table) (name: Name) (elem: ELiteral) =
            match elem, Map.tryFind name metadata.Attributes with
            | ELiteral.LInteger value, Some { Position = position; Type' = _ } ->
                (position, BitConverter.GetBytes value)
            | ELiteral.LUniqueIdentifier value, Some { Position = position; Type' = _ } ->
                let serializedGuid =
                    value.ToString()
                    |> Array.ofSeq
                    |> Array.collect (BitConverter.GetBytes >> Array.take 1) in

                (position, serializedGuid)
            | ELiteral.LVarChar value,
              Some { Position = position
                     Type' = Types.VariableCharacters storageSize } ->
                value.[0 .. (storageSize)]
                |> Seq.map (BitConverter.GetBytes >> fun x -> x.[0])
                |> fun bytes -> Seq.append bytes (blanks 1 (storageSize - value.Length))
                |> fun bytes -> (position, Array.ofSeq bytes)
            | _ -> failwith "Not implemented"

        match Map.tryFind entity schema with
        | Some(Table table) ->
            Map.map (convert table) tuple
            |> Map.toArray
            |> Array.sortBy (snd >> fst)
            |> Array.map (snd >> snd)
            |> Array.concat
            |> Ok
        | None -> Error(ExpressDBError.Serialization "")


    (*
    let mutable warnings = []

    (Map.map
        (fun nameAttr ->
            function
            | ELiteral.LInteger value -> BitConverter.GetBytes value
            | ELiteral.LUniqueIdentifier value ->
                value.ToString()
                |> Seq.map (BitConverter.GetBytes >> fun x -> x.[0])
                |> Array.ofSeq
            | ELiteral.LVarChar value ->
                Map.tryFind entity schema
                |> Option.bind (fun table -> Map.tryFind nameAttr table)
                |> Option.bind (function
                    | AST.Types.VariableCharacters maxRowAllocatedSize ->
                        let inputSize = value.Length |> int64

                        if inputSize > maxRowAllocatedSize then
                            warnings <-
                                List.append
                                    warnings
                                    [ $"Value for field '{nameAttr}' was truncated. Maximum size is {maxRowAllocatedSize} but received {inputSize}." ]

                        value.[0 .. (maxRowAllocatedSize |> int)]
                        |> Seq.map (fun (x: char) -> (BitConverter.GetBytes x).[0])
                        |> Array.ofSeq
                        |> fun bytes ->
                            Array.append
                                bytes
                                [| for _ in 1L .. (maxRowAllocatedSize - inputSize) do
                                       0uy |]
                        |> Some
                    | _ -> None)
                |> Option.defaultValue Array.empty
            | ELiteral.LFixChar value -> value |> Seq.map (BitConverter.GetBytes >> fun x -> x.[0]) |> Array.ofSeq)
        row,
     warnings)
    *)

    let write (page: Page) =
        let path = __SOURCE_DIRECTORY__ + "/../" + page.Entity
        use stream = new IO.FileStream(path, IO.FileMode.OpenOrCreate)
        use binaryStream = new IO.BinaryWriter(stream)

        let syncByState
            { Content = content
              State = pageState
              Size = slotSize
              Position = position }
            =
            match pageState with
            | PageState.Filled ->
                let offset = position * slotSize
                let _ = binaryStream.Seek(offset, SeekOrigin.Begin)

                let blanks =
                    [| for _ in 0 .. (slotSize - 1) do
                           0uy |]

                binaryStream.Write(blanks)
                let _ = binaryStream.Seek(offset, SeekOrigin.Begin)
                binaryStream.Write(content)
            | PageState.Empty -> ()

        page.Content |> Array.iter (syncByState)


[<EntryPoint>]
let main _ =

    /// Hardcoded, this has to come from an actual storage on open()
    let contextSchema: Schema =
        Map.empty
        |> Map.add
            "user"
            (Entity.Table
                { Name = "user"
                  Attributes =
                    Map.empty
                    |> Map.add
                        "name"
                        ({ Position = 0
                           Type' = (AST.Types.VariableCharacters 10) }
                        : Kind.FieldMetadata)
                    |> Map.add
                        "id"
                        ({ Position = 1
                           Type' = AST.Types.UniqueIdentifier })
                    |> Map.add
                        "age"
                        ({ Position = 2
                           Type' = AST.Types.Integer32 })
                    |> Map.add
                        "email"
                        ({ Position = 3
                           Type' = (AST.Types.VariableCharacters 10) }) })

    let createSampleRow (entity: string) (name: string) (age: int) (email: string) =
        Map.empty
        |> Map.add "id" (AST.ELiteral.LUniqueIdentifier(System.Guid.NewGuid()))
        |> Map.add "name" (AST.ELiteral.LVarChar name)
        |> Map.add "age" (AST.ELiteral.LInteger age)
        |> Map.add "email" (AST.ELiteral.LVarChar email)
        |> Tuple.serialize contextSchema "user"
        |> function
            | Ok ba -> ba
            | Error ex -> failwith ex.Message

    let pages =
        let sample = createSampleRow "user" "Wisimar" 1688 "wisimar@email.vd"

        let content =
            { Content = sample
              Position = 0
              Size = Schema.TableByteSize contextSchema.["user"]
              State = PageState.Filled }

        { Entity = "user"
          Content = [| content |] }
        |> Tuple.write

    0
