namespace Language

open Microsoft.FSharp.Reflection
open System.Reflection
open System.Runtime.Serialization

[<RequireQualifiedAccess>]
module Type = begin
    [<KnownType("GetKnownTypes")>]
    type t =
    | TInteger32
    | TVariableString of Size: int32

        member this.ByteSize =
            match this with
            | TInteger32 -> 4
            | TVariableString size -> size

        static member GetKnownTypes() =
            typedefof<t>.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter FSharpType.IsUnion
end

[<RequireQualifiedAccess>]
module Entity = begin
    [<DataContract>]
    type FieldMetadata =
        { [<field: DataMember>]
          fieldPosition: int32
          [<field: DataMember>]
          type': Type.t }

    [<KnownType("GetKnownTypes")>]
    type t =
        | Relation of Map<string, FieldMetadata> * PhysicalOffset: int32

        static member GetKnownTypes() =
            typedefof<t>.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter FSharpType.IsUnion
end

[<RequireQualifiedAccess>]
module Value = begin
    [<DataContract>]
    type t =
        | VInteger32 of int
        | VVariableString of string
end

[<RequireQualifiedAccess>]
module Expression = begin
    type InsertFieldInfo =
        { FieldName: string
          FieldType: Type.t
          FieldValue: Value.t }

    type t =
        | Insert of Name: string * Fields: InsertFieldInfo array
        | CreateRelation of Name: string * Attributes: Map<string, Type.t>
end
