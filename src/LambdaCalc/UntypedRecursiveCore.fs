namespace Jackfoxy.LambdaCalc.UntypedRecursive

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

open Ast
open Jackfoxy.LambdaCalc
open Support.Error

/// Core evaluation functions.
module Core =

    let isYinFix t =
        match t with
        | Abstraction (FI (_, 2, _), "y", _) ->
            true
        | _ ->
            false

    let isBottom t = 
        match t with
        | Abstraction (FI (_, 1, _), _, Abstraction (_, _, Variable (_))) ->
            true
        | _ ->
            false

    let getBottom (ctx : Context) =
        let _, binding = ctx.[ctx.Length - 1]
        match binding with
        | AbbstractionBind bottom ->
            bottom
        | _ ->
            invalidArg "getBottom" "can't get here"

    let rec eval1 ctx t =
        match t with
        | Variable (fi, n, _) ->
            match getBinding fi ctx n with
            | AbbstractionBind t -> 
                t
            | _ -> raise Common.NoRuleAppliesException

        | Application (_, (Abstraction (_, _, t12)), (Abstraction (_) as v2)) ->

            termSubstTop v2 t12

        | Application (fi, (Abstraction (_) as v1), t2) ->

            let t2' = eval1 ctx t2 

            if isBottom t2' && (isYinFix v1) then

                let bottom = getBottom ctx
                Application (fi, bottom, bottom)   
 
            else
                Application (fi, v1, t2')

        | Application (fi, t1, t2) -> 

            let t1' = eval1 ctx t1 

            Application (fi, t1', t2)

        | _ -> raise Common.NoRuleAppliesException
  
    let rec eval ctx t =
        try 
            let t' = eval1 ctx t 
            eval ctx t' 
        with | Common.NoRuleAppliesException -> t
  
    let evalBinding ctx b =
        match b with
        | AbbstractionBind t -> 
            let t' = eval ctx t 
            AbbstractionBind t'
        | bind -> 
            bind
