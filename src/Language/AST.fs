namespace Language

module AST =

    [<RequireQualifiedAccess>]
    type Types =
        | Integer32
        | FixedCharacters of int
        | VariableCharacters of int
        | UniqueIdentifier

        member this.ByteSize =
            match this with
            | Types.Integer32 -> 4
            | Types.FixedCharacters n
            | Types.VariableCharacters n -> n
            | Types.UniqueIdentifier -> 36

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
    type Expression =
        | Insert of Name: string * Fields: InsertFieldInfo array
        | CreateRelation of Name: string * Attributes: Map<string, Types>

    and InsertFieldInfo =
        { FieldName: string
          FieldType: Types
          FieldValue: ELiteral }
