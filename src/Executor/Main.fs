namespace ExpressDB.Executor

module Main =
    open Language.AST
    
    [<EntryPoint>]
    let main _ =
        let createRelationExpr =
            Expression.CreateRelation ("king", Map.empty |> Map.add "name" (Types.VariableCharacters 20))

        let insertRowExpr =
            Expression.Insert ("king", [| {FieldName = "name"; FieldType = (Types.VariableCharacters 20); FieldValue = ELiteral.LVarChar "Gaiseric"} |])

        match Runner.execute createRelationExpr Map.empty with
        | Runner.ExecutionResult.SchemaUpdate schema ->
            match Runner.execute insertRowExpr schema with
            | Runner.ExecutionResult.Effect "INSERT" -> ()
            | _ -> ()
        | _ -> ()

        0
