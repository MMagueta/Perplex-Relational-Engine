namespace Language

module AST =
    [<RequireQualifiedAccess>]
    type Types =
        | Integer32
        | FixedCharacters of int
        | VariableCharacters of int
        | UniqueIdentifier

        member ByteSize: int

    module Kind =
        type FieldMetadata = { Position: int; Type': Types }

        type Table =
            { Name: string
              Attributes: Map<string, FieldMetadata> }

    type Entity = Table of Kind.Table

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
