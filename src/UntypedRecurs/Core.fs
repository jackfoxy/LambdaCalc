namespace Jackfoxy.LambdaCalc.UntypedRecurs

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

open Jackfoxy.LambdaCalc
open CommonAst
open Support.Error

/// Core evaluation functions.
module Core =

    // λy. (λx. f (λy. x x y)) (λx. f (λy. x x y)) y
    let isInnerYcombinator t =
        match t with
        | Abstraction (FI (_), _, 
                        Application (_, 
                            Application (_, 
                                        Abstraction (FI (_), _, 
                                            Application (_, 
                                                Abstraction (FI (_), _, Abstraction (FI (_), _, Application (_))), 
                                                Abstraction (FI (_), _, Application (_))
                                                        )
                                                    ), 
                                        Abstraction (FI (_), _, 
                                            Application (_, 
                                                Abstraction (FI (_), _, Abstraction (FI (_), _, Application (_))), 
                                                Abstraction (FI (_), _, Application (_))
                                                        )
                                                    )
                                        ), 
                            Variable (FI (_), _, _)
                                    )
                      ) ->
            true
        | _ ->
            false

    let isBottom t = 
        match t with
        | Abstraction (FI (_), _, Abstraction (FI (_), _, Variable (FI (_), _, _))) -> 
            true
        | _ ->
            false

    let getIdentity (ctx : Context) =
        let _, binding = ctx.[ctx.Length - 1]
        match binding with
        | AbstractionBind identity ->
            identity
        | _ ->
            invalidArg "getBottom" "can't get here"

    let rec eval1 ctx t =
        match t with
        | Variable (fi, n, _) ->
            match getBinding fi ctx n with
            | AbstractionBind t -> 
                t
            | _ -> raise Common.NoRuleAppliesException

        | Application (_, (Abstraction (_, _, t12)), (Abstraction (_) as v2)) ->

            termSubstTop v2 t12

        | Application (fi, (Abstraction (_) as v1), t2) ->

            let t2' = eval1 ctx t2 

            if  isInnerYcombinator v1 && isBottom t2' then
//                printfn "the Y term"
//                printTerm true ctx v1
//                PrettyPrint.flush()
//                printfn "the outer term"
//                printTerm true ctx t
//                PrettyPrint.flush()
//                printfn "the applicand reduced"
//                printTerm true ctx t2'
//                PrettyPrint.flush()
//                printfn "the applicand"
//                printTerm true ctx t2
//                PrettyPrint.flush()
                getIdentity ctx
 
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
        | AbstractionBind t -> 
            let t' = eval ctx t 
            AbstractionBind t'
        | bind -> 
            bind
