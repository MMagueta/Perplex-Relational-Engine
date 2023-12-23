namespace IO

module Utilities = begin
  [<RequireQualifiedAccess>]
  module String = begin
    let toBytes: string -> byte array =
        Array.ofSeq >> Array.collect (System.BitConverter.GetBytes >> Array.take 1)
  end 
end

module Write = begin
  open Language
  open Utilities

  type Fact = Map<string, Value.t>

  let convertLiteral (relationAttributes: Map<string, Entity.FieldMetadata>) (attributeName: string) (literal: Value.t): int32 * byte array =
    match literal, Map.tryFind attributeName relationAttributes with
    | Value.VVariableString elem, Some {fieldPosition = fieldPosition; type' = Type.TVariableString specificationSize} ->
       let serializedString = String.toBytes elem in
       /// Filling with the rest of the string specified size
       let serializedValue =
           Array.append
               serializedString
               [| for _ in 1..(specificationSize - serializedString.Length) do 0uy |]
       (fieldPosition, serializedValue)
    | Value.VInteger32 elem, Some {fieldPosition = fieldPosition; type' = Type.TInteger32} ->
       let serializedValue = System.BitConverter.GetBytes elem
       (fieldPosition, serializedValue)
    | _, Some _ -> failwith <| Printf.sprintf "Type mismatch for attribute '%s'." attributeName
    | _, None -> failwith <| Printf.sprintf "Attribute '%s' is not part of the relation." attributeName

  let convertFact (schema: Schema.t) (relationName: string) (fact: Fact) =
    match Map.tryFind relationName schema with
    | Some (Entity.Relation (relationAttributes, _)) ->
       Map.fold
         (fun acc key value -> Array.append [| convertLiteral relationAttributes key value |] acc)
         [||]
         fact
       |> Array.sortBy fst
       |> Array.map snd
       |> Array.concat
    | None -> failwithf "Relation '%s' could not be located in the working schema." relationName

  module Disk = begin
    open System
    open System.IO
    
    let writeFact (schema: Schema.t) (relationName: string) (fact: Fact) : unit =
        match Map.tryFind relationName schema with
        | Some (Entity.Relation (relationAttributes, physicalOffset)) ->
            let content = convertFact schema relationName fact
            let relationDefinitionSize = Schema.relationSize relationAttributes in
            let path = "/tmp/perplexdb/" + relationName + ".ndf" in
            use stream = new IO.FileStream(path, IO.FileMode.OpenOrCreate) in
            use binaryStream = new IO.BinaryWriter(stream) in
            // logger.ForContext("ExecutionContext", "Write").Debug($"Position: {position}")
            let offset = physicalOffset * relationDefinitionSize
            let _ = binaryStream.Seek(offset, SeekOrigin.Begin)
            // Fill with blanks
            binaryStream.Write [| for _ in 0..(relationDefinitionSize - 1) do 0uy |]
            let _ = binaryStream.Seek(offset, SeekOrigin.Begin)
            binaryStream.Write content
        | _ -> ()
       
  end  
end

module Read = begin
  open Language

  type ColumnMap = Map<string, Value.t>
  
  let deserialize (schema: Schema.t) (entityName: string) (stream: byte array): ColumnMap =
    match Map.tryFind entityName schema with
    | Some (Entity.Relation (relationAttributes, _)) ->
        let columns  =
          Map.toList relationAttributes
          |> List.sortBy (fun (_, (x: Entity.FieldMetadata)) -> x.fieldPosition)
          |> List.map (fun (name, {type' = type';}) -> (type', name))
        in
        
        let rec reconstruct_columns acc stream = function
          | [] -> acc
          | (type', name)::[] ->
             (name, Value.t.FromBytes type' stream) :: acc
          | (type', name)::typesRest ->
             let ret = (name, Value.t.FromBytes type' stream) :: acc in
             reconstruct_columns ret (stream.[0..(stream.Length - type'.ByteSize)]) typesRest
        in reconstruct_columns [] stream columns |> Map.ofList
    | None -> failwithf "Entity '%s' could not be located in the working schema." entityName
end

