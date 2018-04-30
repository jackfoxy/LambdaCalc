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
    let inline private isInnerYcombinator term =
        match term with
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

    let inline private isBottom term = 
        match term with
        | Abstraction (FI (_), _, Abstraction (FI (_), _, Variable (FI (_), deBruijnIndex, _))) when deBruijnIndex = 0 -> 
            true
        | _ ->
            false

    let private getIdentity (ctx : Context) =
        let _, binding = ctx.[ctx.Length - 1]
        match binding with
        | AbstractionBind identity ->
            identity
        | _ ->
            invalidArg "getBottom" "can't get here"

    let rec eval ctx term =
        match term with
        | Variable (fileInfo, n, _) ->
            match getBinding fileInfo ctx n with
            | AbstractionBind t -> 
                t
            | _ -> 
                term

        | Application (_, (Abstraction (_, _, t12)), (Abstraction (_) as v2)) ->
            termSubstTop v2 t12

        | Application (fileInfo, (Abstraction (_) as v1), t2) ->
            let t2' = eval ctx t2 

            match isInnerYcombinator v1, isBottom t2' with
            | true, true ->
                termSubstTop v1 <| getIdentity ctx
            | _ ->
                Application (fileInfo, v1, t2')

        | Application (fileInfo, t1, t2) -> 
            Application (fileInfo, (eval ctx t1), t2)

        | _ -> 
            term
