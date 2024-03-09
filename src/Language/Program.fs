namespace PerplexDB.Language

module Main =
    open System.IO
    open FSharp.Text.Lexing

    let generateAST text =
        let lexbuf = LexBuffer<char>.FromString text
        let ast = Parser.start Lexer.tokenStream lexbuf
        ast

    [<EntryPoint>]
    let main argv =
        generateAST "CREATE RELATION Person (Name VARCHAR(10) Age INTEGER)"
        |> printfn "%A"

        generateAST "INSERT Person (Name VARCHAR(10) \"Marcos\" Age INTEGER 24)"
        |> printfn "%A"

        generateAST "PROJECT (Name VARCHAR(10)) Person"
        |> printfn "%A"

        generateAST "PROJECT SUM(Age INTEGER) Person"
        |> printfn "%A"

        generateAST "PROJECT ALL Person"
        |> printfn "%A"

        generateAST "UPDATE Account SET AccountNumber (PROJECT SUM(Value INTEGER) Credit SELECT AccountNumber = 1 - PROJECT SUM(Value INTEGER) Debit SELECT AccountNumber = 1) SELECT AccountNumber = 1"
        |> printfn "%A"

        if argv.Length > 0 then
            Array.map (System.IO.File.ReadAllText >> generateAST) argv |> printfn "%A"
        else
            ()

        0
