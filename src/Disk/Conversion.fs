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

  open System
  open System.Runtime.InteropServices

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
             let ret = (name, Value.t.FromBytes type' stream.[0..(type'.ByteSize - 1)]) :: acc in
             reconstruct_columns ret (stream.[type'.ByteSize..]) typesRest
        in reconstruct_columns [] stream columns |> Map.ofList
    | None -> failwithf "Entity '%s' could not be located in the working schema." entityName

  [<RequireQualifiedAccessAttribute>]
  module Tree =
      [<DllImport(__SOURCE_DIRECTORY__ + "/libbplustree.so", CallingConvention = CallingConvention.StdCall, ExactSpelling = true)>]
      extern void print_leaves(void* _tree);
      [<DllImport(__SOURCE_DIRECTORY__ + "/libbplustree.so", CallingConvention = CallingConvention.StdCall, ExactSpelling = true)>]
      extern void* insert(void* _tree, int _key, int _value)
      [<DllImport(__SOURCE_DIRECTORY__ + "/libbplustree.so", CallingConvention = CallingConvention.StdCall, ExactSpelling = true)>]
      extern void find_and_print(void * _tree, int _key, bool _verbose)
      [<DllImport(__SOURCE_DIRECTORY__ + "/libbplustree.so", CallingConvention = CallingConvention.StdCall, ExactSpelling = true)>]
      extern int find_and_get_value(void * _tree, int _key, bool _verbose)  
  
  [<Struct>]
  type Page = 
      { instances: byte array array
        indexes: (int * int) array } // For now, maps a int key to a array position

  let buildPages initialPageNumber instancesPerPage pageBound (reader: IO.FileStream) instanceSize amountAlreadyRead attributeToIndex schema entityName (relationToIndex: Map<string,Entity.FieldMetadata>) =
      let mutable buffer = [|for _ in 1..8_000 do 0uy|]
      let mutable amountAlreadyRead = amountAlreadyRead
      let pages =
        [| for pageNumber in initialPageNumber..instancesPerPage..pageBound do
            let amountToRead =
                if instancesPerPage * instanceSize > ((reader.Length |> int32) - amountAlreadyRead)
                then ((reader.Length |> int32) - amountAlreadyRead)
                else instancesPerPage * instanceSize
            amountAlreadyRead <- amountAlreadyRead + amountToRead
            reader.Read(buffer, pageNumber*instanceSize, amountToRead) |> ignore
            let instances = buffer |> Array.chunkBySize instanceSize |> Array.take pageBound
            let indexes =
                instances
                |> Array.mapi (fun i stream ->
                    Map.tryFind attributeToIndex (deserialize schema entityName stream)
                    |> function Some (Value.VInteger32 value) -> (value, i)
                              | Some _ -> failwithf "Currently, indexes on '%A' are not supported. Halting pagination." relationToIndex.[attributeToIndex].type'
                              | None -> failwithf "Attribute '%A' could not be found. Halting pagination." attributeToIndex)
            yield { instances = instances
                    indexes = indexes }
          done |]
      pages, amountAlreadyRead
      
  type Pagination =
      { pages: Page array; tree: nativeint; amountAlreadyRead: int }
  
  let buildPagination schema entityName (relationToIndex: Map<string, Entity.FieldMetadata>) attributeToIndex initialPageNumber amountAlreadyRead =
      let instanceSize = Schema.relationSize relationToIndex
      let instancesPerPage: int32 = 8_000 / instanceSize
      use reader = System.IO.File.OpenRead $"/tmp/perplexdb/{entityName}.ndf"
      reader.Seek (0, System.IO.SeekOrigin.Begin) |> ignore
      let maxPagesAtOnce = 64
      let pages, amountAlreadyRead = buildPages initialPageNumber instancesPerPage maxPagesAtOnce (reader: IO.FileStream) instanceSize amountAlreadyRead attributeToIndex schema entityName relationToIndex
      let tree = Array.fold (fun tree elem -> Array.fold (fun tree (key, offset) -> Tree.insert(tree, key, offset)) tree elem.indexes) IntPtr.Zero pages
      { pages = pages; tree = tree; amountAlreadyRead = amountAlreadyRead }

  let searchOntologicalKey () = // (entityToLoad: string) (key: Value.t) =
     // let tree = insert(IntPtr.Zero, 1, 10)
     // let tree = insert(tree, 1, 10)
     // let tree = insert(tree, 2, 11)
     // let tree = insert(tree, 3, 12)
     // let tree = insert(tree, 4, 13)
     // let tree = insert(tree, 7, 17)
     // let tree = insert(tree, 6, 23)
     // let tree = insert(tree, 10, 73)
     // let tree = insert(tree, 5, 39)
     // let tree = insert(tree, 11, 12)
     // print_leaves(tree)
     ()

  let search (schema: Schema.t) (entityName: string) predicate = //: ColumnMap =
      match Map.tryFind entityName schema with
      | Some (Entity.Relation (relationAttributes, physicalCount)) ->
          
          let initialPageNumber = 0
          let mutable amountAlreadyRead = 0
          
          let lastReadChunk = buildPagination schema entityName relationAttributes "Age" initialPageNumber amountAlreadyRead
          printfn "Leaves: %A" lastReadChunk.pages
          Tree.print_leaves lastReadChunk.tree
          amountAlreadyRead <- lastReadChunk.amountAlreadyRead

          match Tree.find_and_get_value (lastReadChunk.tree, predicate, false) with
          | -1 -> None
          | offset ->
              lastReadChunk.pages.[0].instances.[offset]
              |> deserialize schema entityName
              |> Some
          
      | None -> failwithf "Entity '%s' could not be located in the working schema." entityName

  [<EntryPoint>]
  let main _ =
      searchOntologicalKey()
      0
end
