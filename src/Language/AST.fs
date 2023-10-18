namespace Language

module AST =
    open Microsoft.FSharp.Reflection
    open System.Reflection
    open System.Runtime.Serialization
        
    [<RequireQualifiedAccess>]
    [<KnownType("GetKnownTypes")>]
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
        static member GetKnownTypes() =
            typedefof<Types>.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) 
            |> Array.filter FSharpType.IsUnion
    
    module Kind =
        [<DataContract>]
        type FieldMetadata = { [<field : DataMember>] Position: int; [<field : DataMember>] Type': Types }

        [<DataContract>]
        type Table =
            { [<field : DataMember>] Name: string
              [<field : DataMember>] RowCount: int
              [<field : DataMember>] Attributes: Map<string, FieldMetadata> }

    [<KnownType("GetKnownTypes")>]
    type Entity =
        | Table of Kind.Table
        static member GetKnownTypes() =
            typedefof<Entity>.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) 
            |> Array.filter FSharpType.IsUnion

    [<DataContract>]
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
