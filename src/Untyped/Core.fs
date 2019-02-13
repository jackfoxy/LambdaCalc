namespace Jackfoxy.LambdaCalc.Untyped

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

open Jackfoxy.LambdaCalc
open CommonAst

/// Core evaluation functions.
module Core =

    let rec eval ctx term _ =
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
            Application (fileInfo, v1, (eval ctx t2 id))

        | Application (fileInfo, t1, t2) -> 
            Application (fileInfo, (eval ctx t1 id), t2)

        | _ -> 
            term
