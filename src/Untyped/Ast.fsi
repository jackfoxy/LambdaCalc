namespace Jackfoxy.LambdaCalc.Untyped

open Support.Error
open Jackfoxy.LambdaCalc
open CommonAst

/// Syntax trees and associated support functions.
module Ast =

    val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

    val printBinding : ctx : Context -> b : Binding -> unit
