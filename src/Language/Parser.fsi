// Signature file for parser generated by fsyacc
module Parser
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
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> ( Expression.t option ) 
