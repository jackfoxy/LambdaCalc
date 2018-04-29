namespace Jackfoxy.LambdaCalc.Untyped

open Jackfoxy.LambdaCalc

/// Syntax tree print functions.
module Ast =

    val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

    val printBinding : ctx : Context -> b : Binding -> unit
