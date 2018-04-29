namespace Jackfoxy.LambdaCalc.Untyped

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

open Support.Error
open Jackfoxy.LambdaCalc
open CommonAst
open PrettyPrint

/// Syntax trees and associated support functions.
module Ast =

    let small term = 
        match term with 
        | Variable (_) -> true 
        | _ -> false
  
    let index2Name fileInfo (ctx : Context) i =
        try 
            let (xn, _) = List.item i ctx 
            xn
        with
        | Failure _ ->
          let msg =
            Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
          error fileInfo (msg i (List.length ctx))

    let rec printtmTerm outer ctx term =
        match term with
        | Abstraction (_, name, term2) ->
            let (ctx', name') = pickfreshname ctx name
            pr <| sprintf "lambda %s." name'
            if (small term2) && (not outer) then printSpace ()
            printtmTerm outer ctx' term2;
        | t -> 
            printApplicationTerm outer ctx t
    and printApplicationTerm outer ctx term =
        match term with
        | Application (_, term1, term2) ->
            printApplicationTerm false ctx term1
            printSpace ()
            printTerm false ctx term2
        | t -> 
            printTerm outer ctx t
    and printTerm outer (ctx : Context) term =
        match term with
        | Variable (fileInfo, deBruinIndex, contextLength) ->
          if (ctxLength ctx) = contextLength
          then 
            pr (index2Name fileInfo ctx deBruinIndex)
          else
            pr <| sprintf "[bad index: %s/%s in {%s }]"
                    (string deBruinIndex)
                    (string contextLength)
                    (List.fold (fun s (x, _) -> sprintf "%s  %s" s x) "" ctx)
        | term -> 
            pr "("
            printtmTerm outer ctx term
            pr ")"
  
    let printBinding ctx binding =
        match binding with 
        | NameBind -> () 
        | AbstractionBind term -> 
            pr "= "
            printtmTerm true ctx term
  