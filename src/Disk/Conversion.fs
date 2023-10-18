﻿module PerplexDB.Pager.PhysicalStorage

open System
open System.IO
open Language.AST

[<Literal>]
let PAGE_SIZE = 50

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

type PageHeader =
    { StartingPosition : int
      EndingPosition : int }

type Page =
    { Content: PageSlot array
      Entity: Name
      Header: PageHeader }
    static member New(): Page =
        { Content = [||]
          Entity = "";
          Header = { StartingPosition = 0
                     EndingPosition = PAGE_SIZE } }

[<RequireQualifiedAccessAttribute>]
module PerplexDBError =
    exception Serialization of string

let blanks lowerBound upperBound =
    seq {
        for _ in lowerBound..upperBound do
            0uy
    }

module Row =

    let private serializeLiteral (logger: Serilog.ILogger) (metadata: Kind.Table) (name: Name) (elem: ELiteral) =
        match elem, Map.tryFind name metadata.Attributes with
        | LInteger value, Some { Position = position; Type' = _ } ->
            (position, BitConverter.GetBytes value)
        | LUniqueIdentifier value, Some { Position = position; Type' = _ } ->
            let serializedGuid =
                value.ToString()
                |> Array.ofSeq
                |> Array.collect (BitConverter.GetBytes >> Array.take 1) in
            (position, serializedGuid)
        | LVarChar value,
          Some { Position = position
                 Type' = Types.VariableCharacters storageSize } ->
            if value.Length > storageSize
            then logger.ForContext("ExecutionContext", "Serialization").Warning("Value for field '{a}' was truncated. Maximum size is {b}, however received {c}.", name, storageSize, value.Length)
            value.[0 .. (storageSize)]
            |> Seq.map (BitConverter.GetBytes >> fun x -> x.[0])
            |> fun bytes -> Seq.append bytes (blanks 1 (storageSize - value.Length))
            |> fun bytes -> (position, Array.ofSeq bytes)
        | _ -> failwith "Not implemented"

    let serialize (logger: Serilog.ILogger) (schema: Schema) (entity: Name) (tuple: Map<Name, ELiteral>) : Result<byte array, exn> =
        match Map.tryFind entity schema with
        | Some(Table table) ->
            Map.map (serializeLiteral logger table) tuple
            |> Map.toArray
            |> Array.sortBy (snd >> fst)
            |> Array.map (snd >> snd)
            |> Array.concat
            |> Ok
        | None -> Error(PerplexDBError.Serialization "")

    let write (logger: Serilog.ILogger) (position: int) (slotSize: int) (entity: string) (content: byte array) =
        let path = __SOURCE_DIRECTORY__ + "/../" + entity
        use stream = new IO.FileStream(path, IO.FileMode.OpenOrCreate)
        use binaryStream = new IO.BinaryWriter(stream)
        
        logger.ForContext("ExecutionContext", "Write").Debug($"Position: {position}")
        let offset = position * slotSize
        let _ = binaryStream.Seek(offset, SeekOrigin.Begin)
        binaryStream.Write(blanks 0 (slotSize - 1) |> Array.ofSeq)
        let _ = binaryStream.Seek(offset, SeekOrigin.Begin)
        binaryStream.Write(content)

    let loadPage (schema: Schema) (entityName: string) (page: Page) =
        let path = __SOURCE_DIRECTORY__ + "/../" + entityName
        use stream = new IO.FileStream(path, IO.FileMode.Open)
        use binaryStream = new IO.BinaryReader(stream)
        let entityBlockSize = Schema.TableByteSize schema.[entityName]
        let amountOfRows = System.Math.Floor (PAGE_SIZE / entityBlockSize |> decimal) |> int
        let _ = stream.Seek(page.Header.StartingPosition, SeekOrigin.Begin)
        binaryStream.ReadBytes amountOfRows

    let pageLifter (schema: Schema) (entityName: string): Page array =
        [| for _ in 0..2 do Page.New() |]
