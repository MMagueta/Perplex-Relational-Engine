structure AST = 
struct
    datatype Type 
        = Integer32 
        | VariableChars of int    
        
    structure Kind =
    struct
        type FieldMetadata = { Position: int, Type': Type }

        type Table = { Attributes: (string, FieldMetadata) Dictionary.dict }
    end

    datatype Entity = Table of Kind.Table

    datatype ELiteral = LInteger of int | LVarChar of string

    type InsertFieldInfo = { FieldName: string, FieldType: Type, FieldValue: ELiteral }
    
    datatype Expression 
        = Insert of string * InsertFieldInfo array
        | CreateRelation of string * (string, Type) Dictionary.dict
end

fun test x = print (x ^ "\n")
