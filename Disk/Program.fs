module SolDB.Pager.PhysicalStorage

open System
open System.Runtime.InteropServices
open System.IO
open SolQL
open SolQL.AST

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
         |> Map.add "name" (AST.Types.VariableCharacters 128)
         |> Map.add "id" AST.Types.UniqueIdentifier
         |> Map.add "age" AST.Types.Integer32)

(*
let store (path: string) =
    use stream = new IO.FileStream(path, IO.FileMode.Append)
    use binaryStream = new IO.BinaryWriter(stream)
    let test = "Hello World!" |> Text.Encoding.ASCII.GetBytes
    binaryStream.Write(test)
    
let load (path: string) =
    use stream = new IO.FileStream(path, IO.FileMode.Open)
    use binaryStream = new IO.BinaryReader(stream)
    binaryStream.ReadBytes PAGE_SIZE 
    |> Text.Encoding.ASCII.GetString
*)

/// For now, no validation on what is provided
/// In the future, check the sizes with the schema
/// Example: VARCHAR(128) -> received "abc", fill a array[128] with "abc" + blanks
let serialize (row: Map<string, AST.ELiteral>) =
    Map.map
        (fun _ ->
            function
            | ELiteral.LInteger value -> BitConverter.GetBytes value
            | ELiteral.LUniqueIdentifier value -> value.ToString() |> Seq.map BitConverter.GetBytes |> Array.concat
            | ELiteral.LVarChar value -> value |> Seq.map BitConverter.GetBytes |> Array.concat
            | ELiteral.LFixChar value -> value |> Seq.map BitConverter.GetBytes |> Array.concat)
        row

let flush (page: Page) =
    match page.Slot with
    | PageSlot.Filled(offset, false) ->
        let path: string = __SOURCE_DIRECTORY__ + "/../" + page.File
        use stream = new IO.FileStream(path, IO.FileMode.OpenOrCreate)
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
    
    let row =
        Map.empty
        |> Map.add "id" (AST.ELiteral.LUniqueIdentifier(System.Guid.NewGuid()))
        |> Map.add "name" (AST.ELiteral.LVarChar "Balderic")
        |> Map.add "age" (AST.ELiteral.LInteger 23)
        |> serialize
        |> Map.toArray
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.concat

    let page =
        { Page = row
          File = "Test.db"
          Slot = PageSlot.Filled(0, false)
          Size = row.Length }

    flush page
    

    
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
    

    0
