module ExpressDB.Pager.PhysicalStorage

open System
open System.Runtime.InteropServices
open System.IO
open Language
open Language.AST

type Schema = Map<Name, Table>
and Name = string
and Table = Map<Name, AST.Types>

type PageSlot =
    | Filled of StartingOffset: int * ``SyncedOnDisk?``: bool
    | Empty of StartingOffset: int

type Page =
    { Page: byte array
      Entity: string
      Slot: PageSlot
      Size: int }

[<Literal>]
let PAGE_SIZE = 4096

/// Hardcoded, this has to come from an actual storage on open()
let contextSchema: Schema =
    Map.empty
    |> Map.add
        "user"
        (Map.empty
         |> Map.add "name" (AST.Types.VariableCharacters 5)
         |> Map.add "id" AST.Types.UniqueIdentifier
         |> Map.add "age" AST.Types.Integer32)


/// For now, no validation on what is provided
/// In the future, check the sizes with the schema
/// Example: VARCHAR(128) -> received "abc", fill a array[128] with "abc" + blanks
let serialize (schema: Schema) (entity: string) (row: Map<string, AST.ELiteral>) =
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


(*
let readBlock (offset: int) (size: int) (path: string) =
    use stream = new IO.FileStream(path, IO.FileMode.Open)
    do stream.Seek(offset, SeekOrigin.Begin)
    use binaryStream = new IO.BinaryReader(stream)
    binaryStream.ReadBytes PAGE_SIZE 
    |> Seq.map BitConverter.ToChar
*)
let flush (page: Page) =
    match page.Slot with
    | PageSlot.Filled(offset, false) ->
        let path: string = __SOURCE_DIRECTORY__ + "/../" + page.Entity
        use stream = new IO.FileStream(path, IO.FileMode.OpenOrCreate)
        use binaryStream = new IO.BinaryWriter(stream)
        let _ = binaryStream.Seek(offset * page.Size, SeekOrigin.Begin)

        let blank =
            [| for _ in 0 .. (page.Size - 1) do
                   0uy |]

        binaryStream.Write(blank)
        let _ = binaryStream.Seek(offset * page.Size, SeekOrigin.Begin)
        binaryStream.Write(page.Page)
    | PageSlot.Filled(_, true) -> failwith "Page clean"
    | _ -> failwith "Page empty"

(*
module BTreeCreate =
    [<DllImport(__SOURCE_DIRECTORY__ + "/Tree.so")>]
    extern IntPtr CreateBTree(int level)

    let Invoke = CreateBTree

module BTreeInsert =
    [<DllImport(__SOURCE_DIRECTORY__ + "/Tree.so")>]
    extern void insert(IntPtr tree, int value)

    let Invoke = insert

module BTreeSearch =
    [<DllImport(__SOURCE_DIRECTORY__ + "/Tree.so")>]
    extern int* search(IntPtr tree, int value)

    let Invoke = search
*)

[<EntryPoint>]
let main _ =

    let createSampleRow (entity: string) (name: string) (age: int) =
        Map.empty
        //|> Map.add "id" (AST.ELiteral.LUniqueIdentifier(System.Guid.NewGuid()))
        |> Map.add "name" (AST.ELiteral.LVarChar name)
        //|> Map.add "age" (AST.ELiteral.LInteger age)
        |> serialize contextSchema entity
        |> fun (map, warnings) ->
            Seq.iter (printfn "%A") warnings
            map
        |> Map.toArray
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.concat

    // printfn "%A" ((createSampleRow "Volferic  " 34).Length)

    let pages =
        [ { Page = createSampleRow "user" "Balderic" 23
            Entity = "Test.db"
            Slot = PageSlot.Filled(0, false)
            Size = 20 }
          { Page = createSampleRow "user" "Aleric" 17
            Entity = "Test.db"
            Slot = PageSlot.Filled(1, false)
            Size = 20 }
          { Page = createSampleRow "user" "Bolemeric" 31
            Entity = "Test.db"
            Slot = PageSlot.Filled(2, false)
            Size = 20 }
          { Page = createSampleRow "user" "Volferic" 34
            Entity = "Test.db"
            Slot = PageSlot.Filled(3, false)
            Size = 20 } ]

    pages |> Seq.iter flush

    (*
    let tree: IntPtr = BTreeCreate.Invoke(3)
    BTreeInsert.Invoke(tree, 1)
    BTreeInsert.Invoke(tree, 2)
    BTreeInsert.Invoke(tree, 3)
    BTreeInsert.Invoke(tree, 4)
    BTreeInsert.Invoke(tree, 5)
    BTreeInsert.Invoke(tree, 6)
    BTreeInsert.Invoke(tree, 7)
    
    let pointer = BTreeSearch.Invoke(tree, 3)
    NativeInterop.NativePtr.get pointer 0
    |> printfn "%A"
    
    *)
    0
