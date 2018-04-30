namespace Jackfoxy.LambdaCalc

open Common

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

    val evalDriver : eval : (Context -> Term -> Term) -> ctx : Context -> term : Term -> Term

    /// Evaluate binding
    val evalBinding : eval : (Context -> Term -> Term) -> ctx : Context ->  binding : Binding -> Binding

    val reduceInput : reduceParams : ReduceParams -> ctx : Context ->  cmds : Command list -> Context
