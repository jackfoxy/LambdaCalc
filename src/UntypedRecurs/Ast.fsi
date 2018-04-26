namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Support.Error
open Jackfoxy.LambdaCalc

/// Syntax trees and associated support functions.
module Ast =

    val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

   // val printtmTerm : outer : bool -> ctx : Context -> t : Term -> unit

    val printBinding : ctx : Context -> b : Binding -> unit
