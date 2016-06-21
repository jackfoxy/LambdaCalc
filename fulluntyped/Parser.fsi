// Signature file for parser generated by fsyacc
module Parser
type token = 
  | VBAR of (Support.Error.Info)
  | USCORE of (Support.Error.Info)
  | TRIANGLE of (Support.Error.Info)
  | STAR of (Support.Error.Info)
  | SLASH of (Support.Error.Info)
  | SEMI of (Support.Error.Info)
  | RSQUARE of (Support.Error.Info)
  | RPAREN of (Support.Error.Info)
  | RCURLY of (Support.Error.Info)
  | LT of (Support.Error.Info)
  | LSQUAREBAR of (Support.Error.Info)
  | LSQUARE of (Support.Error.Info)
  | LPAREN of (Support.Error.Info)
  | LEFTARROW of (Support.Error.Info)
  | LCURLYBAR of (Support.Error.Info)
  | LCURLY of (Support.Error.Info)
  | HASH of (Support.Error.Info)
  | GT of (Support.Error.Info)
  | EXISTS of (Support.Error.Info)
  | EQEQ of (Support.Error.Info)
  | EQ of (Support.Error.Info)
  | EOF of (Support.Error.Info)
  | DOT of (Support.Error.Info)
  | DDARROW of (Support.Error.Info)
  | DARROW of (Support.Error.Info)
  | COMMA of (Support.Error.Info)
  | COLONHASH of (Support.Error.Info)
  | COLONEQ of (Support.Error.Info)
  | COLONCOLON of (Support.Error.Info)
  | COLON of (Support.Error.Info)
  | BARRSQUARE of (Support.Error.Info)
  | BARRCURLY of (Support.Error.Info)
  | BARGT of (Support.Error.Info)
  | BANG of (Support.Error.Info)
  | ARROW of (Support.Error.Info)
  | DQUOTE of (Support.Error.Info)
  | APOSTROPHE of (Support.Error.Info)
  | STRINGV of (string Support.Error.WithInfo)
  | FLOATV of (float Support.Error.WithInfo)
  | INTV of (int Support.Error.WithInfo)
  | LCID of (string Support.Error.WithInfo)
  | UCID of (string Support.Error.WithInfo)
  | IN of (Support.Error.Info)
  | LET of (Support.Error.Info)
  | ISZERO of (Support.Error.Info)
  | PRED of (Support.Error.Info)
  | SUCC of (Support.Error.Info)
  | TIMESFLOAT of (Support.Error.Info)
  | LAMBDA of (Support.Error.Info)
  | FALSE of (Support.Error.Info)
  | TRUE of (Support.Error.Info)
  | ELSE of (Support.Error.Info)
  | THEN of (Support.Error.Info)
  | IF of (Support.Error.Info)
type tokenId = 
    | TOKEN_VBAR
    | TOKEN_USCORE
    | TOKEN_TRIANGLE
    | TOKEN_STAR
    | TOKEN_SLASH
    | TOKEN_SEMI
    | TOKEN_RSQUARE
    | TOKEN_RPAREN
    | TOKEN_RCURLY
    | TOKEN_LT
    | TOKEN_LSQUAREBAR
    | TOKEN_LSQUARE
    | TOKEN_LPAREN
    | TOKEN_LEFTARROW
    | TOKEN_LCURLYBAR
    | TOKEN_LCURLY
    | TOKEN_HASH
    | TOKEN_GT
    | TOKEN_EXISTS
    | TOKEN_EQEQ
    | TOKEN_EQ
    | TOKEN_EOF
    | TOKEN_DOT
    | TOKEN_DDARROW
    | TOKEN_DARROW
    | TOKEN_COMMA
    | TOKEN_COLONHASH
    | TOKEN_COLONEQ
    | TOKEN_COLONCOLON
    | TOKEN_COLON
    | TOKEN_BARRSQUARE
    | TOKEN_BARRCURLY
    | TOKEN_BARGT
    | TOKEN_BANG
    | TOKEN_ARROW
    | TOKEN_DQUOTE
    | TOKEN_APOSTROPHE
    | TOKEN_STRINGV
    | TOKEN_FLOATV
    | TOKEN_INTV
    | TOKEN_LCID
    | TOKEN_UCID
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_ISZERO
    | TOKEN_PRED
    | TOKEN_SUCC
    | TOKEN_TIMESFLOAT
    | TOKEN_LAMBDA
    | TOKEN_FALSE
    | TOKEN_TRUE
    | TOKEN_ELSE
    | TOKEN_THEN
    | TOKEN_IF
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__starttoplevel
    | NONTERM_toplevel
    | NONTERM_Command
    | NONTERM_Binder
    | NONTERM_Term
    | NONTERM_AppTerm
    | NONTERM_PathTerm
    | NONTERM_ATerm
    | NONTERM_Fields
    | NONTERM_NEFields
    | NONTERM_Field
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val toplevel : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( Toplevel ) 
