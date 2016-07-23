(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Core evaluation functions.
module Core

open Ast
open FSharpTapl

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
            sprintf "%s %s in parent %s" info.ApplicatorName info.ApplicandName.Value  info.ParentAbstractionName

    match msg with
    | Some x ->
        printfn "%s -- %s" applicationMsg x
    | None ->
        printfn "%s" applicationMsg

(* ------------------------   EVALUATION  ------------------------ *)

let isFixY t =
    match t with
    | Application (_, (Abstraction (fi, name, _)), v2) -> 
        match fi with
        | Support.Error.Info.FI (_, line, character) when line = 19 && character = 32 ->
            name = "y"
        | _ -> false
        
    | _ -> false

// Bottom = λ t. λ b. b
let isBottom t =            //test for bottom is incorrect, redo
    match t with
    | Abstraction (_, _, Abstraction (_, _, Variable (_))) ->
        true
    | _ ->
        false
  
let rec eval1 ctx t =
    match t with
    | Variable (fi, n, _) ->
        match getBinding fi ctx n with
        | AbbstractionBind t -> t
        | _ -> raise Common.NoRuleAppliesException

    | Application (_, (Abstraction (_, _, t12)), (Abstraction (_) as v2)) ->
        if (isFixY t) then

            match t with
            | Application (_, t1, t2) ->
                Some "fix Y is value for termSubstTop"
                |> printApplication ctx t1 t2
                printfn ""
            | _ -> invalidArg "" ""

        if isBottom v2 then
            printfn ">>bottom"  //we never seem to catch bottom here when Fix Y

        termSubstTop v2 t12

    | Application (fi, (Abstraction (_) as v1), t2) ->

        if isBottom t2 then
            Some "bottom before eval of applicand"
            |> printApplication ctx v1 t2 

        let t2' = eval1 ctx t2 

        if (isFixY t) then
            Some "fix Y, eval applicand"
            |> printApplication ctx v1 t2'

        if isBottom t2' then
            Some "bottom after eval of applicand"
            |> printApplication ctx v1 t2'           

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
