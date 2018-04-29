namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Jackfoxy.LambdaCalc

/// Syntax tree print functions.
module Ast =

    val printTerm : outer : bool -> ctx : Context -> term : Term -> unit

    val printBinding : ctx : Context -> binding : Binding -> unit
