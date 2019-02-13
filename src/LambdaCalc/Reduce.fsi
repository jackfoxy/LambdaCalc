namespace Jackfoxy.LambdaCalc

open Common
open Continuation

/// Lambda reduction to normal form through call by value.
module Reduce =

    type ReduceParams =
        {
        AddBinding : Context -> string -> Binding -> Context
        Eval : Context -> Term -> Term
        EvalBinding : Context -> Binding -> Binding
        PrintTerm : bool -> Context -> Term -> unit
        PrintBinding : Context -> Binding -> unit
        InputLines : InputLines list
        }

    val evalDriver : eval : (Context -> Term -> (('T -> 'T) -> Term)) -> ctx : Context -> term : Term -> Term

    /// Evaluate binding
    val evalBinding : eval : (Context -> Term -> (('T -> 'T) -> Term)) -> ctx : Context ->  binding : Binding -> Binding

    val reduceInput : reduceParams : ReduceParams -> ctx : Context ->  cmds : Command list -> Context
