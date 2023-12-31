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
module Value = begin
    open System
    [<DataContract>]
    type t =
        | VInteger32 of int32
        | VVariableString of string
        static member FromBytes (expected: Type.t) (stream: byte array): t =
            match expected with
            | Type.TInteger32 ->
                if stream.Length = 4 then
                    System.BitConverter.ToInt32 stream
                    |> VInteger32
                else failwithf "Int32 expects bytes of size 4, got: %d" stream.Length
            | Type.TVariableString size ->
                if stream.Length = size then
                    VVariableString (Text.Encoding.UTF8.GetString stream)
                else failwithf "String(%d) received the wrong size: %d" size stream.Length
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
module Expression = begin
    type InsertFieldInfo =
        { FieldName: string
          FieldType: Type.t
          FieldValue: Value.t }

    type t =
        | Insert of Name: string * Fields: InsertFieldInfo array
        | CreateRelation of Name: string * Attributes: Map<string, Type.t>
end

[<RequireQualifiedAccess>]
module Schema = begin
  type t = Map<string, Entity.t>

  let relationSize (relationAttributes: Map<string, Entity.FieldMetadata>) : int32 =
      relationAttributes
      |> Map.fold (fun acc _ ({type' = type'}: Entity.FieldMetadata) -> acc + type'.ByteSize) 0l
end

