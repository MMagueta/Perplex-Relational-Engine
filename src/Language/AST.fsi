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
              RowCount: int
              Attributes: Map<string, FieldMetadata> }

    type Entity =
        | Table of Kind.Table
        
    type ELiteral =
        | LInteger of int
        | LVarChar of string
        | LFixChar of string
        | LUniqueIdentifier of System.Guid

    [<RequireQualifiedAccess>]
    type Expression =
        | Insert of Name: string * Fields: InsertFieldInfo array
        | CreateRelation of Name: string * Attributes: Map<string, Types>

    and InsertFieldInfo =
        { FieldName: string
          FieldType: Types
          FieldValue: ELiteral }
