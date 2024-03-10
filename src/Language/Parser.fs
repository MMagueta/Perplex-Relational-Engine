// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open Language

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | RIGHT_LIM
  | LEFT_LIM
  | EOF
  | AT
  | GTE
  | CONSTRAINT
  | TAKING
  | RESTRICT
  | END
  | BEGIN
  | LOCK_WRITE
  | LOCK_READ
  | SET
  | ALL
  | SUM
  | NOT
  | PLUS
  | MINUS
  | EQUAL
  | SELECT
  | PROJECT
  | UPDATE
  | INSERT
  | RELATION
  | CREATE
  | TYPE' of (string)
  | LITERAL_STRING of (string)
  | LITERAL_INTEGER of (int)
  | IDENTIFIER of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_RIGHT_LIM
    | TOKEN_LEFT_LIM
    | TOKEN_EOF
    | TOKEN_AT
    | TOKEN_GTE
    | TOKEN_CONSTRAINT
    | TOKEN_TAKING
    | TOKEN_RESTRICT
    | TOKEN_END
    | TOKEN_BEGIN
    | TOKEN_LOCK_WRITE
    | TOKEN_LOCK_READ
    | TOKEN_SET
    | TOKEN_ALL
    | TOKEN_SUM
    | TOKEN_NOT
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_EQUAL
    | TOKEN_SELECT
    | TOKEN_PROJECT
    | TOKEN_UPDATE
    | TOKEN_INSERT
    | TOKEN_RELATION
    | TOKEN_CREATE
    | TOKEN_TYPE'
    | TOKEN_LITERAL_STRING
    | TOKEN_LITERAL_INTEGER
    | TOKEN_IDENTIFIER
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_File
    | NONTERM_Statement
    | NONTERM_RestrictionQuery
    | NONTERM_Constraint
    | NONTERM_Operator
    | NONTERM_Refinement
    | NONTERM_LocalizedIdentifier
    | NONTERM_ListAttributes
    | NONTERM_Rev_Attributes
    | NONTERM_ListValues
    | NONTERM_Rev_Values
    | NONTERM_ListStatements
    | NONTERM_RevStatements
    | NONTERM_end

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | RIGHT_LIM  -> 0 
  | LEFT_LIM  -> 1 
  | EOF  -> 2 
  | AT  -> 3 
  | GTE  -> 4 
  | CONSTRAINT  -> 5 
  | TAKING  -> 6 
  | RESTRICT  -> 7 
  | END  -> 8 
  | BEGIN  -> 9 
  | LOCK_WRITE  -> 10 
  | LOCK_READ  -> 11 
  | SET  -> 12 
  | ALL  -> 13 
  | SUM  -> 14 
  | NOT  -> 15 
  | PLUS  -> 16 
  | MINUS  -> 17 
  | EQUAL  -> 18 
  | SELECT  -> 19 
  | PROJECT  -> 20 
  | UPDATE  -> 21 
  | INSERT  -> 22 
  | RELATION  -> 23 
  | CREATE  -> 24 
  | TYPE' _ -> 25 
  | LITERAL_STRING _ -> 26 
  | LITERAL_INTEGER _ -> 27 
  | IDENTIFIER _ -> 28 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_RIGHT_LIM 
  | 1 -> TOKEN_LEFT_LIM 
  | 2 -> TOKEN_EOF 
  | 3 -> TOKEN_AT 
  | 4 -> TOKEN_GTE 
  | 5 -> TOKEN_CONSTRAINT 
  | 6 -> TOKEN_TAKING 
  | 7 -> TOKEN_RESTRICT 
  | 8 -> TOKEN_END 
  | 9 -> TOKEN_BEGIN 
  | 10 -> TOKEN_LOCK_WRITE 
  | 11 -> TOKEN_LOCK_READ 
  | 12 -> TOKEN_SET 
  | 13 -> TOKEN_ALL 
  | 14 -> TOKEN_SUM 
  | 15 -> TOKEN_NOT 
  | 16 -> TOKEN_PLUS 
  | 17 -> TOKEN_MINUS 
  | 18 -> TOKEN_EQUAL 
  | 19 -> TOKEN_SELECT 
  | 20 -> TOKEN_PROJECT 
  | 21 -> TOKEN_UPDATE 
  | 22 -> TOKEN_INSERT 
  | 23 -> TOKEN_RELATION 
  | 24 -> TOKEN_CREATE 
  | 25 -> TOKEN_TYPE' 
  | 26 -> TOKEN_LITERAL_STRING 
  | 27 -> TOKEN_LITERAL_INTEGER 
  | 28 -> TOKEN_IDENTIFIER 
  | 31 -> TOKEN_end_of_input
  | 29 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_start 
    | 3 -> NONTERM_File 
    | 4 -> NONTERM_File 
    | 5 -> NONTERM_File 
    | 6 -> NONTERM_File 
    | 7 -> NONTERM_Statement 
    | 8 -> NONTERM_Statement 
    | 9 -> NONTERM_Statement 
    | 10 -> NONTERM_Statement 
    | 11 -> NONTERM_Statement 
    | 12 -> NONTERM_Statement 
    | 13 -> NONTERM_Statement 
    | 14 -> NONTERM_RestrictionQuery 
    | 15 -> NONTERM_Constraint 
    | 16 -> NONTERM_Constraint 
    | 17 -> NONTERM_Operator 
    | 18 -> NONTERM_Refinement 
    | 19 -> NONTERM_Refinement 
    | 20 -> NONTERM_LocalizedIdentifier 
    | 21 -> NONTERM_ListAttributes 
    | 22 -> NONTERM_ListAttributes 
    | 23 -> NONTERM_Rev_Attributes 
    | 24 -> NONTERM_Rev_Attributes 
    | 25 -> NONTERM_Rev_Attributes 
    | 26 -> NONTERM_Rev_Attributes 
    | 27 -> NONTERM_ListValues 
    | 28 -> NONTERM_ListValues 
    | 29 -> NONTERM_Rev_Values 
    | 30 -> NONTERM_Rev_Values 
    | 31 -> NONTERM_Rev_Values 
    | 32 -> NONTERM_Rev_Values 
    | 33 -> NONTERM_ListStatements 
    | 34 -> NONTERM_ListStatements 
    | 35 -> NONTERM_RevStatements 
    | 36 -> NONTERM_RevStatements 
    | 37 -> NONTERM_end 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 31 
let _fsyacc_tagOfErrorTerminal = 29

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | RIGHT_LIM  -> "RIGHT_LIM" 
  | LEFT_LIM  -> "LEFT_LIM" 
  | EOF  -> "EOF" 
  | AT  -> "AT" 
  | GTE  -> "GTE" 
  | CONSTRAINT  -> "CONSTRAINT" 
  | TAKING  -> "TAKING" 
  | RESTRICT  -> "RESTRICT" 
  | END  -> "END" 
  | BEGIN  -> "BEGIN" 
  | LOCK_WRITE  -> "LOCK_WRITE" 
  | LOCK_READ  -> "LOCK_READ" 
  | SET  -> "SET" 
  | ALL  -> "ALL" 
  | SUM  -> "SUM" 
  | NOT  -> "NOT" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | EQUAL  -> "EQUAL" 
  | SELECT  -> "SELECT" 
  | PROJECT  -> "PROJECT" 
  | UPDATE  -> "UPDATE" 
  | INSERT  -> "INSERT" 
  | RELATION  -> "RELATION" 
  | CREATE  -> "CREATE" 
  | TYPE' _ -> "TYPE'" 
  | LITERAL_STRING _ -> "LITERAL_STRING" 
  | LITERAL_INTEGER _ -> "LITERAL_INTEGER" 
  | IDENTIFIER _ -> "IDENTIFIER" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | RIGHT_LIM  -> (null : System.Object) 
  | LEFT_LIM  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | AT  -> (null : System.Object) 
  | GTE  -> (null : System.Object) 
  | CONSTRAINT  -> (null : System.Object) 
  | TAKING  -> (null : System.Object) 
  | RESTRICT  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | BEGIN  -> (null : System.Object) 
  | LOCK_WRITE  -> (null : System.Object) 
  | LOCK_READ  -> (null : System.Object) 
  | SET  -> (null : System.Object) 
  | ALL  -> (null : System.Object) 
  | SUM  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | SELECT  -> (null : System.Object) 
  | PROJECT  -> (null : System.Object) 
  | UPDATE  -> (null : System.Object) 
  | INSERT  -> (null : System.Object) 
  | RELATION  -> (null : System.Object) 
  | CREATE  -> (null : System.Object) 
  | TYPE' _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | LITERAL_STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | LITERAL_INTEGER _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | IDENTIFIER _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;1us;65535us;0us;2us;5us;65535us;0us;15us;5us;6us;7us;8us;12us;120us;119us;121us;6us;65535us;0us;16us;5us;16us;7us;16us;12us;16us;48us;49us;119us;16us;1us;65535us;71us;72us;1us;65535us;82us;83us;7us;65535us;27us;28us;34us;35us;37us;38us;58us;59us;67us;68us;70us;71us;80us;81us;1us;65535us;83us;84us;2us;65535us;20us;21us;24us;25us;2us;65535us;20us;93us;24us;93us;1us;65535us;41us;42us;1us;65535us;41us;104us;1us;65535us;12us;13us;1us;65535us;12us;119us;2us;65535us;0us;4us;2us;3us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;3us;5us;11us;18us;20us;22us;30us;32us;35us;38us;40us;42us;44us;46us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;1us;1us;1us;1us;1us;2us;1us;3us;1us;3us;1us;4us;1us;4us;1us;5us;1us;5us;1us;5us;1us;5us;1us;5us;1us;5us;1us;6us;1us;7us;1us;8us;1us;8us;1us;8us;1us;8us;1us;8us;1us;8us;4us;9us;10us;11us;14us;1us;9us;1us;9us;1us;9us;1us;9us;1us;9us;1us;10us;1us;10us;1us;10us;1us;10us;1us;10us;1us;10us;1us;10us;1us;11us;1us;11us;1us;11us;1us;12us;1us;12us;1us;12us;1us;12us;1us;12us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;13us;1us;14us;1us;14us;1us;14us;1us;14us;1us;14us;1us;14us;1us;14us;1us;14us;1us;14us;1us;16us;1us;16us;1us;16us;1us;17us;1us;19us;1us;19us;1us;19us;1us;19us;1us;20us;1us;20us;1us;20us;3us;22us;25us;26us;2us;23us;24us;2us;23us;24us;1us;23us;1us;23us;1us;23us;2us;25us;26us;2us;25us;26us;1us;26us;1us;26us;1us;26us;3us;28us;31us;32us;2us;29us;30us;2us;29us;30us;1us;29us;1us;29us;1us;29us;1us;29us;1us;30us;2us;31us;32us;2us;31us;32us;1us;31us;1us;32us;1us;32us;1us;32us;1us;32us;2us;34us;36us;1us;35us;1us;36us;1us;37us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;4us;6us;8us;10us;12us;14us;16us;18us;20us;22us;24us;26us;28us;30us;32us;34us;36us;38us;40us;42us;44us;46us;51us;53us;55us;57us;59us;61us;63us;65us;67us;69us;71us;73us;75us;77us;79us;81us;83us;85us;87us;89us;91us;93us;95us;97us;99us;101us;103us;105us;107us;109us;111us;113us;115us;117us;119us;121us;123us;125us;127us;129us;131us;133us;135us;137us;139us;141us;143us;145us;147us;149us;151us;153us;155us;157us;159us;161us;163us;165us;167us;169us;171us;173us;175us;177us;179us;181us;183us;185us;187us;189us;193us;196us;199us;201us;203us;205us;208us;211us;213us;215us;217us;221us;224us;227us;229us;231us;233us;235us;237us;240us;243us;245us;247us;249us;251us;253us;256us;258us;260us;|]
let _fsyacc_action_rows = 123
let _fsyacc_actionTableElements = [|8us;32768us;2us;122us;9us;9us;10us;7us;11us;5us;20us;23us;21us;44us;22us;39us;24us;17us;0us;49152us;1us;32768us;2us;122us;0us;16385us;0us;16386us;4us;32768us;20us;23us;21us;44us;22us;39us;24us;17us;0us;16387us;4us;32768us;20us;23us;21us;44us;22us;39us;24us;17us;0us;16388us;1us;32768us;28us;10us;1us;32768us;28us;11us;1us;32768us;28us;12us;4us;16417us;20us;23us;21us;44us;22us;39us;24us;17us;1us;32768us;8us;14us;0us;16389us;0us;16390us;0us;16391us;1us;32768us;23us;18us;1us;32768us;28us;19us;1us;32768us;1us;20us;1us;16405us;28us;94us;1us;32768us;0us;22us;0us;16392us;4us;32768us;1us;24us;6us;74us;13us;36us;14us;29us;1us;16405us;28us;94us;1us;32768us;0us;26us;1us;32768us;28us;27us;1us;16402us;19us;86us;0us;16393us;1us;32768us;1us;30us;1us;32768us;28us;31us;1us;32768us;25us;32us;1us;32768us;0us;33us;1us;32768us;28us;34us;1us;16402us;19us;86us;0us;16394us;1us;32768us;28us;37us;1us;16402us;19us;86us;0us;16395us;1us;32768us;28us;40us;1us;32768us;1us;41us;1us;16411us;28us;105us;1us;32768us;0us;43us;0us;16396us;1us;32768us;28us;45us;1us;32768us;12us;46us;1us;32768us;28us;47us;1us;32768us;1us;48us;1us;32768us;20us;73us;1us;32768us;17us;50us;1us;32768us;1us;51us;1us;32768us;20us;52us;1us;32768us;14us;53us;1us;32768us;1us;54us;1us;32768us;28us;55us;1us;32768us;25us;56us;1us;32768us;0us;57us;1us;32768us;28us;58us;1us;16402us;19us;86us;1us;32768us;17us;60us;1us;32768us;20us;61us;1us;32768us;14us;62us;1us;32768us;1us;63us;1us;32768us;28us;64us;1us;32768us;25us;65us;1us;32768us;0us;66us;1us;32768us;28us;67us;1us;16402us;19us;86us;1us;32768us;0us;69us;1us;32768us;0us;70us;1us;16402us;19us;86us;1us;16399us;5us;82us;0us;16397us;1us;32768us;6us;74us;1us;32768us;27us;75us;1us;32768us;1us;76us;1us;32768us;28us;77us;1us;32768us;25us;78us;1us;32768us;0us;79us;1us;32768us;28us;80us;1us;16402us;19us;86us;0us;16398us;1us;32768us;4us;85us;1us;32768us;28us;90us;0us;16400us;0us;16401us;1us;32768us;28us;87us;1us;32768us;18us;88us;1us;32768us;27us;89us;0us;16403us;1us;32768us;3us;91us;1us;32768us;28us;92us;0us;16404us;1us;16406us;28us;99us;1us;32768us;25us;95us;1us;16408us;1us;96us;1us;32768us;27us;97us;1us;32768us;0us;98us;0us;16407us;1us;32768us;25us;100us;1us;16409us;1us;101us;1us;32768us;27us;102us;1us;32768us;0us;103us;0us;16410us;1us;16412us;28us;112us;1us;32768us;25us;106us;2us;32768us;1us;107us;27us;111us;1us;32768us;27us;108us;1us;32768us;0us;109us;1us;32768us;26us;110us;0us;16413us;0us;16414us;1us;32768us;25us;113us;2us;32768us;1us;115us;27us;114us;0us;16415us;1us;32768us;27us;116us;1us;32768us;0us;117us;1us;32768us;26us;118us;0us;16416us;4us;16418us;20us;23us;21us;44us;22us;39us;24us;17us;0us;16419us;0us;16420us;0us;16421us;|]
let _fsyacc_actionTableRowOffsets = [|0us;9us;10us;12us;13us;14us;19us;20us;25us;26us;28us;30us;32us;37us;39us;40us;41us;42us;44us;46us;48us;50us;52us;53us;58us;60us;62us;64us;66us;67us;69us;71us;73us;75us;77us;79us;80us;82us;84us;85us;87us;89us;91us;93us;94us;96us;98us;100us;102us;104us;106us;108us;110us;112us;114us;116us;118us;120us;122us;124us;126us;128us;130us;132us;134us;136us;138us;140us;142us;144us;146us;148us;150us;151us;153us;155us;157us;159us;161us;163us;165us;167us;168us;170us;172us;173us;174us;176us;178us;180us;181us;183us;185us;186us;188us;190us;192us;194us;196us;197us;199us;201us;203us;205us;206us;208us;210us;213us;215us;217us;219us;220us;221us;223us;226us;227us;229us;231us;233us;234us;239us;240us;241us;|]
let _fsyacc_reductionSymbolCounts = [|1us;2us;1us;2us;2us;6us;1us;1us;6us;6us;8us;4us;5us;29us;9us;0us;3us;1us;0us;4us;3us;0us;1us;5us;2us;3us;6us;0us;1us;6us;3us;4us;7us;0us;1us;1us;2us;1us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;1us;2us;2us;2us;2us;3us;3us;3us;3us;3us;3us;3us;4us;5us;5us;6us;7us;7us;8us;9us;9us;10us;10us;10us;10us;11us;11us;12us;12us;12us;12us;13us;13us;14us;14us;15us;|]
let _fsyacc_immediateActions = [|65535us;49152us;65535us;16385us;16386us;65535us;16387us;65535us;16388us;65535us;65535us;65535us;65535us;65535us;16389us;16390us;16391us;65535us;65535us;65535us;65535us;65535us;16392us;65535us;65535us;65535us;65535us;65535us;16393us;65535us;65535us;65535us;65535us;65535us;65535us;16394us;65535us;65535us;16395us;65535us;65535us;65535us;65535us;16396us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16397us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16398us;65535us;65535us;16400us;16401us;65535us;65535us;65535us;16403us;65535us;65535us;16404us;65535us;65535us;65535us;65535us;65535us;16407us;65535us;65535us;65535us;65535us;16410us;65535us;65535us;65535us;65535us;65535us;65535us;16413us;16414us;65535us;65535us;16415us;65535us;65535us;65535us;16416us;65535us;16419us;16420us;16421us;|]
let _fsyacc_reductions = lazy [|
# 285 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  Expression.t option  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startstart));
# 294 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_File in
            let _2 = parseState.GetInput(2) :?> 'gentype_end in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                                       _1 
                   )
# 51 "Parser.fsy"
                 :  Expression.t option ));
# 306 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_end in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                       None 
                   )
# 52 "Parser.fsy"
                 :  Expression.t option ));
# 317 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_Statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "Parser.fsy"
                                              Some (Expression.LockRead _2) 
                   )
# 55 "Parser.fsy"
                 : 'gentype_File));
# 328 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_Statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                               Some (Expression.LockWrite _2) 
                   )
# 56 "Parser.fsy"
                 : 'gentype_File));
# 339 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            let _4 = parseState.GetInput(4) :?> string in
            let _5 = parseState.GetInput(5) :?> 'gentype_ListStatements in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                                                           Some (Expression.Begin ([_2; _3; _4],_5))
                   )
# 57 "Parser.fsy"
                 : 'gentype_File));
# 353 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                    Some _1 
                   )
# 58 "Parser.fsy"
                 : 'gentype_File));
# 364 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_RestrictionQuery in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                              _1 
                   )
# 61 "Parser.fsy"
                 : 'gentype_Statement));
# 375 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> string in
            let _5 = parseState.GetInput(5) :?> 'gentype_ListAttributes in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Parser.fsy"
                            Expression.CreateRelation (_3, Map.ofList _5) 
                   )
# 63 "Parser.fsy"
                 : 'gentype_Statement));
# 387 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> 'gentype_ListAttributes in
            let _5 = parseState.GetInput(5) :?> string in
            let _6 = parseState.GetInput(6) :?> 'gentype_Refinement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                   Expression.Project (_5, List.map (fun ((identifier: string), _type') -> identifier) _3
                                                           |> Expression.ProjectionParameter.Restrict, _6) 
                   )
# 65 "Parser.fsy"
                 : 'gentype_Statement));
# 401 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _4 = parseState.GetInput(4) :?> string in
            let _5 = parseState.GetInput(5) :?> string in
            let _7 = parseState.GetInput(7) :?> string in
            let _8 = parseState.GetInput(8) :?> 'gentype_Refinement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "Parser.fsy"
                                   Expression.Project (_7, Expression.ProjectionParameter.Sum _4, _8) 
                   )
# 68 "Parser.fsy"
                 : 'gentype_Statement));
# 415 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> string in
            let _4 = parseState.GetInput(4) :?> 'gentype_Refinement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                   Expression.Project (_3, Expression.ProjectionParameter.All, _4) 
                   )
# 70 "Parser.fsy"
                 : 'gentype_Statement));
# 427 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> 'gentype_ListValues in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "Parser.fsy"
                            Expression.Insert (_2, List.map (fun ((identifier: string), type', value) -> { FieldName = identifier; FieldType = type'; FieldValue = value }: Expression.InsertFieldInfo) _4 |> Array.ofList) 
                   )
# 72 "Parser.fsy"
                 : 'gentype_Statement));
# 439 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> string in
            let _6 = parseState.GetInput(6) :?> 'gentype_RestrictionQuery in
            let _12 = parseState.GetInput(12) :?> string in
            let _13 = parseState.GetInput(13) :?> string in
            let _15 = parseState.GetInput(15) :?> string in
            let _16 = parseState.GetInput(16) :?> 'gentype_Refinement in
            let _21 = parseState.GetInput(21) :?> string in
            let _22 = parseState.GetInput(22) :?> string in
            let _24 = parseState.GetInput(24) :?> string in
            let _25 = parseState.GetInput(25) :?> 'gentype_Refinement in
            let _28 = parseState.GetInput(28) :?> 'gentype_Refinement in
            let _29 = parseState.GetInput(29) :?> 'gentype_Constraint in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Parser.fsy"
                            Expression.Update (_2, {FieldName = _4; FieldType = Type.TInteger32; FieldValue = Expression.Minus (_6, Expression.Minus (Expression.Project (_15, Expression.ProjectionParameter.Sum _12, _16), Expression.Project (_24, Expression.ProjectionParameter.Sum _21, _25)))}, _28, _29) 
                   )
# 75 "Parser.fsy"
                 : 'gentype_Statement));
# 462 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> int in
            let _5 = parseState.GetInput(5) :?> string in
            let _6 = parseState.GetInput(6) :?> string in
            let _8 = parseState.GetInput(8) :?> string in
            let _9 = parseState.GetInput(9) :?> 'gentype_Refinement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "Parser.fsy"
                               let projection = Expression.ProjectionParameter.Taking (_3, [_5])
                               Expression.Project (_8, projection, _9) 
                   )
# 86 "Parser.fsy"
                 : 'gentype_RestrictionQuery));
# 478 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "Parser.fsy"
                             None 
                   )
# 90 "Parser.fsy"
                 : 'gentype_Constraint));
# 488 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_Operator in
            let _3 = parseState.GetInput(3) :?> 'gentype_LocalizedIdentifier in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "Parser.fsy"
                                                                     Some <| (_2, [_3]) 
                   )
# 91 "Parser.fsy"
                 : 'gentype_Constraint));
# 500 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "Parser.fsy"
                                 Expression.FGte 
                   )
# 94 "Parser.fsy"
                 : 'gentype_Operator));
# 510 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 97 "Parser.fsy"
                                 None 
                   )
# 97 "Parser.fsy"
                 : 'gentype_Refinement));
# 520 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 99 "Parser.fsy"
                                   Some <| Expression.Operators.Equal (_2, _4) 
                   )
# 99 "Parser.fsy"
                 : 'gentype_Refinement));
# 532 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 103 "Parser.fsy"
                                   Expression.LocalizedIdentifier (_1, _3) 
                   )
# 103 "Parser.fsy"
                 : 'gentype_LocalizedIdentifier));
# 544 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 106 "Parser.fsy"
                           [] 
                   )
# 106 "Parser.fsy"
                 : 'gentype_ListAttributes));
# 554 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Rev_Attributes in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 107 "Parser.fsy"
                                          List.rev _1 
                   )
# 107 "Parser.fsy"
                 : 'gentype_ListAttributes));
# 565 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 111 "Parser.fsy"
                           match _2 with
                           | "VARCHAR" -> [(_1, Type.TVariableString _4)] 
                   )
# 111 "Parser.fsy"
                 : 'gentype_Rev_Attributes));
# 579 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _2 = parseState.GetInput(2) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 114 "Parser.fsy"
                           match _2 with
                           | "INTEGER" -> [(_1, Type.TInteger32)]
                           | "VARCHAR" -> failwith "VARCHAR is a parametric type, therefore it requires a size." 
                   )
# 114 "Parser.fsy"
                 : 'gentype_Rev_Attributes));
# 593 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Rev_Attributes in
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 117 "Parser.fsy"
                                                           (_2, match _3 with
                                                                | "INTEGER" -> Type.TInteger32
                                                                | "VARCHAR" -> failwith "VARCHAR is a parametric type, therefore it requires a size.") :: _1 
                   )
# 117 "Parser.fsy"
                 : 'gentype_Rev_Attributes));
# 608 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Rev_Attributes in
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            let _5 = parseState.GetInput(5) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 121 "Parser.fsy"
                           (_2, match _3 with
                                | "VARCHAR" -> Type.TVariableString _5) :: _1 
                   )
# 121 "Parser.fsy"
                 : 'gentype_Rev_Attributes));
# 623 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 125 "Parser.fsy"
                           [] 
                   )
# 125 "Parser.fsy"
                 : 'gentype_ListValues));
# 633 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Rev_Values in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 126 "Parser.fsy"
                                      List.rev _1 
                   )
# 126 "Parser.fsy"
                 : 'gentype_ListValues));
# 644 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> int in
            let _6 = parseState.GetInput(6) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 130 "Parser.fsy"
                           match _2 with
                           | "VARCHAR" -> [(_1, Type.TVariableString _4, Value.VVariableString _6)] 
                   )
# 130 "Parser.fsy"
                 : 'gentype_Rev_Values));
# 659 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 133 "Parser.fsy"
                           match _2 with
                           | "INTEGER" -> [(_1, Type.TInteger32, Value.VInteger32 _3)]
                           | "VARCHAR" -> failwith "VARCHAR is a parametric type, therefore it requires a size." 
                   )
# 133 "Parser.fsy"
                 : 'gentype_Rev_Values));
# 674 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Rev_Values in
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            let _4 = parseState.GetInput(4) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 137 "Parser.fsy"
                           (_2, (match _3 with
                                 | "INTEGER" -> Type.TInteger32
                                 | "VARCHAR" -> failwith "VARCHAR is a parametric type, therefore it requires a size."), Value.VInteger32 _4) :: _1 
                   )
# 137 "Parser.fsy"
                 : 'gentype_Rev_Values));
# 690 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Rev_Values in
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            let _5 = parseState.GetInput(5) :?> int in
            let _7 = parseState.GetInput(7) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 141 "Parser.fsy"
                           (_2, (match _3 with
                                 | "VARCHAR" -> Type.TVariableString _5), Value.VVariableString _7) :: _1 
                   )
# 141 "Parser.fsy"
                 : 'gentype_Rev_Values));
# 706 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 145 "Parser.fsy"
                           [] 
                   )
# 145 "Parser.fsy"
                 : 'gentype_ListStatements));
# 716 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_RevStatements in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 146 "Parser.fsy"
                                         List.rev _1 
                   )
# 146 "Parser.fsy"
                 : 'gentype_ListStatements));
# 727 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 150 "Parser.fsy"
                           [_1] 
                   )
# 150 "Parser.fsy"
                 : 'gentype_RevStatements));
# 738 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_RevStatements in
            let _2 = parseState.GetInput(2) :?> 'gentype_Statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 152 "Parser.fsy"
                           _2 :: _1 
                   )
# 152 "Parser.fsy"
                 : 'gentype_RevStatements));
# 750 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 155 "Parser.fsy"
                                None 
                   )
# 155 "Parser.fsy"
                 : 'gentype_end));
|]
# 761 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions = _fsyacc_reductions.Value;
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 32;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  Expression.t option  =
    engine lexer lexbuf 0 :?> _
