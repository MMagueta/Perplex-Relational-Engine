namespace Language

module AST =
    [<RequireQualifiedAccess>]
    type Types =
        | Integer32
        | FixedCharacters of int64
        | VariableCharacters of int64
        | UniqueIdentifier

        member ByteSize: int64

    type ELiteral =
        | LInteger of int
        | LVarChar of string
        | LFixChar of string
        | LUniqueIdentifier of System.Guid

    [<RequireQualifiedAccess>]
    type Expressions = Insert of Fields: InsertFieldInfo array

    and InsertFieldInfo =
        { FieldName: string
          FieldType: Types
          FieldValue: ELiteral }
