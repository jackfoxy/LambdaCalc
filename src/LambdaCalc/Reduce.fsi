﻿namespace Jackfoxy.LambdaCalc

open Common
open PrettyPrint

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

    val reduceInput : reduceParams : ReduceParams -> ctx : Context ->  cmds : Command list -> Context