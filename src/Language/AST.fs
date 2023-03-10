namespace Language

module AST = 

    [<RequireQualifiedAccess>]
    type Types =
        | Integer32
        | FixedCharacters of int64
        | VariableCharacters of int64
        | UniqueIdentifier
        member this.ByteSize: int64 =
            match this with
            | Types.Integer32 -> 4
            | Types.FixedCharacters n
            | Types.VariableCharacters n -> n
            | Types.UniqueIdentifier -> 36
            
    type ELiteral =
        | LInteger of int
        | LVarChar of string
        | LFixChar of string
        | LUniqueIdentifier of System.Guid
            
    [<RequireQualifiedAccess>]
    type Expressions =
        | Insert of Fields: InsertFieldInfo array
    and InsertFieldInfo =
        { FieldName: string
          FieldType: Types
          FieldValue: ELiteral }
