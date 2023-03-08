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
      File: string
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
         |> Map.add "name" (AST.Types.VariableCharacters 8)
         |> Map.add "id" AST.Types.UniqueIdentifier
         |> Map.add "age" AST.Types.Integer32)


/// For now, no validation on what is provided
/// In the future, check the sizes with the schema
/// Example: VARCHAR(128) -> received "abc", fill a array[128] with "abc" + blanks
let serialize (schema: Schema) (entity: string) (row: Map<string, AST.ELiteral>) =
    Map.map
        (fun nameAttr ->
            function
            | ELiteral.LInteger value -> BitConverter.GetBytes value
            | ELiteral.LUniqueIdentifier value -> value.ToString() |> Seq.map BitConverter.GetBytes |> Array.concat
            | ELiteral.LVarChar value ->
                Map.tryFind entity schema
                |> fun x -> printfn "1: %A" x; x
                |> Option.bind (fun table -> Map.tryFind nameAttr table)
                |> fun x -> printfn "2: %A" x; x
                |> Option.bind (function
                    | AST.Types.VariableCharacters size ->
                        let length = value.Length |> int64

                        if length <= size then
                            Array.append
                                (value.ToCharArray())
                                [| for _ in 1L .. (size - length) do
                                       '\000' |]
                            |> Seq.map BitConverter.GetBytes
                            |> Array.concat
                            |> Some
                        else None
                            //failwith "Value size is greater than the assigned to the field"
                    | _ -> //failwith "Type mismatch"
                        None)
                |> fun x -> printfn "3: %A" x; x
                |> Option.defaultValue [||]
            | ELiteral.LFixChar value -> value |> Seq.map BitConverter.GetBytes |> Array.concat)
        row
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
        let path: string = __SOURCE_DIRECTORY__ + "/../" + page.File
        try IO.File.Delete path
        with _ -> ()
        use stream = new IO.FileStream(path, IO.FileMode.Create)
        use binaryStream = new IO.BinaryWriter(stream)
        let _ = binaryStream.Seek(offset * page.Size, SeekOrigin.Begin)
        binaryStream.Write(page.Page)
    | PageSlot.Filled(_, true) -> failwith "Page clean"
    | _ -> failwith "Page empty"

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

[<EntryPoint>]
let main _ =

    let createSampleRow (entity: string) (name: string) (age: int) =
        Map.empty
        //|> Map.add "id" (AST.ELiteral.LUniqueIdentifier(System.Guid.NewGuid()))
        |> Map.add "name" (AST.ELiteral.LVarChar name)
        //|> Map.add "age" (AST.ELiteral.LInteger age)
        |> serialize contextSchema entity
        |> Map.toArray
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.concat

    // printfn "%A" ((createSampleRow "Volferic  " 34).Length)

    let pages =
        [ { Page = createSampleRow "user" "Balderic" 23
            File = "Test.db"
            Slot = PageSlot.Filled(0, false)
            Size = 20 }
          { Page = createSampleRow "user" "Aleric" 17
            File = "Test.db"
            Slot = PageSlot.Filled(1, false)
            Size = 20 }
          { Page = createSampleRow "user" "Bolemeric" 31
            File = "Test.db"
            Slot = PageSlot.Filled(2, false)
            Size = 20 }
          { Page = createSampleRow "user" "Volferic" 34
            File = "Test.db"
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
