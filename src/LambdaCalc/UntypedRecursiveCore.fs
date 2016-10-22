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
open Jackfoxy.LambdaCalc.Compatability

/// Core evaluation functions.
module Core =

    type ApplicationInfo =
        {
        ParentAbstractionName : string
        ParentAbstraction : Term
        ApplicatorName : string
        Applicator : Term
        ApplicandName : string option
        Applicand : Term
        ApplicandIsApplication : bool
        }

    let lineFromFileInfo fileInfo =
        match fileInfo with
        | Support.Error.Info.FI (_, line, _) -> line
        | _-> invalidArg "" ""

    let getParentAbstraction (ctx : Context) fileInfo =

        let searchLine = lineFromFileInfo fileInfo

        let x' =
            ctx
            |> List.find (fun x -> 
                match snd x with
                | AbbstractionBind (Abstraction (fi, _, _)) ->
                    (lineFromFileInfo fi) = searchLine
                |_ -> invalidArg "" "")

        match snd x' with
        | AbbstractionBind (Abstraction (_) as parentAbstraction) ->
            (fst x'), parentAbstraction
        | _ -> invalidArg "" ""
    
    let getApplicationInfo ctx applicator applicand =
        match applicator with
        | Abstraction (fileInfo, applicatorName, _) ->
            let bindingName, parentAbstraction = getParentAbstraction ctx fileInfo
            match applicand with
            | Variable (_) ->
                {
                ParentAbstractionName = bindingName
                ParentAbstraction = parentAbstraction
                ApplicatorName = applicatorName
                Applicator = applicator
                ApplicandName = None
                Applicand = applicand
                ApplicandIsApplication = false
                }
            | Abstraction (_, applicandName, _) ->
                {
                ParentAbstractionName = bindingName
                ParentAbstraction = parentAbstraction
                ApplicatorName = applicatorName
                Applicator = applicator
                ApplicandName = Some applicandName
                Applicand = applicand
                ApplicandIsApplication = false
                }
            | Application (_) ->
                {
                ParentAbstractionName = bindingName
                ParentAbstraction = parentAbstraction
                ApplicatorName = applicatorName
                Applicator = applicator
                ApplicandName = None
                Applicand = applicand
                ApplicandIsApplication = true
                }
        | _ -> invalidArg "" ""

    let isYinFix ctx applicator applicand =
        let info = 
            getApplicationInfo ctx applicator applicand
        match info.ApplicatorName, info.ApplicandName.Value, info.ParentAbstractionName with
        | "y", "t", "fix" -> true
        | _ -> false

    let printApplication ctx applicator applicand msg =
        let info = 
            getApplicationInfo ctx applicator applicand

        let applicationMsg = 
            match info.Applicand with
            | Variable (fi,  deBruinIndex, contextLength) ->
                sprintf "%s %s in parent %s" info.ApplicatorName (sprintf "Variable(line %i, index %i ctxLength %i)" (lineFromFileInfo fi) deBruinIndex contextLength) info.ParentAbstractionName
            | Application (_) ->
                sprintf "%s %s in parent %s" info.ApplicatorName "Application()" info.ParentAbstractionName
            | _ ->
                sprintf "%s %s in parent %s" info.ApplicatorName info.ApplicandName.Value info.ParentAbstractionName

        match msg with
        | Some x ->
            let z = sprintf "\n%s -- %s" applicationMsg x

            if z = "\ny t in parent fix -- bottom after eval of applicand" then 
                z |> pr
             else ()

        | None -> ()
    //        sprintf "\n%s" applicationMsg
    //        |> pr

    (* ------------------------   EVALUATION  ------------------------ *)

    let isFixY t =
        match t with
        | Application (_, (Abstraction (fi, name, _)), v2) -> 
            match fi with
            | Support.Error.Info.FI (_, line, character) when line = 19 ->
                sprintf "fix name %s character %i" name character
                |> pr
                true
            | _ -> false
        
        | _ -> false

    // Bottom = λ t. λ b. b
    let isBottom t =            //test for bottom is incorrect, redo
        match t with
        | Abstraction (_, _, Abstraction (_, _, Variable (_))) ->
            true
        | _ ->
            false
  
    let getAbstraction (ctx : Context) name =
        let bottom =
            ctx
            |> List.find(fun t -> fst t = name)
        match snd bottom with
        | AbbstractionBind x -> x
        | _ -> invalidArg "" ""

    let rec eval1 ctx t =
        match t with
        | Variable (fi, n, _) ->
            match getBinding fi ctx n with
            | AbbstractionBind t -> 
                t
            | _ -> raise Common.NoRuleAppliesException

        | Application (_, (Abstraction (_, _, t12)), (Abstraction (_) as v2)) ->

    //        if isBottom v2 then
    //            sprintf "\n>>bottom\n"  //we never seem to catch bottom here when Fix Y
    //            |> pr
    //            printtmTerm true ctx t

            termSubstTop v2 t12

        | Application (fi, (Abstraction (_) as v1), t2) ->

            let t2' = eval1 ctx t2 

            try
                if isBottom t2' && (isYinFix ctx v1 t2') then
            
    //                Some "bottom after eval of applicand"
    //                |> printApplication ctx v1 t2'

    //                printfn "\n"
    //                printfn "\nevaluated applicand          outer t2'>"
    //                printfn "\n%A" t2'
    //                printtmTerm true ctx t2'
    //                printfn "\n"
    //                printfn "\nevaluated applicand          inner t2'>"
    //                printtmTerm false ctx t2'
    //                printfn "\n"
    //                printfn "\nbefore evaluation applicand   outer t2>"
    //                printfn "\n%A" t2
    //                printtmTerm true ctx t2
    //                printfn "\n"
    //                printfn "\nbefore evaluation applicand   inner t2>"
    //                printtmTerm false ctx t2
    //                printfn "\n"
    //                printfn "\nterm                           outer t>"
    //                printfn "\n%A" t
    //                printtmTerm true ctx t 
    //                printfn "\n"
    //                printfn "\nterm                           inner t>"
    //                printtmTerm false ctx t 


        //            bottomAbstraction ctx //bad index error
                    let _, binding = ctx.[ctx.Length - 1]
                    match binding with
                    | AbbstractionBind bottom ->
                        Application (fi, bottom, bottom)   // expected test result
    //                   bottom // test result = 3, bad index divisor 1 higher that above
    //                    bottom |> eval1 ctx  // fails on "no rule"

                   // Application (fi, (getAbstraction ctx "id"), (getAbstraction ctx "bottom" |> eval1 ctx))  //stack overflow

                
                     
                else
                    Application (fi, v1, t2')
            with e ->
                printfn "\nfailed try> %s" e.Message
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
