namespace Jackfoxy.LambdaCalc.UntypedRecurs

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

    (* Printing *)
    (* The printing functions call these utility functions to insert grouping
      information the pretty-printing library
    *)

    let small t = 
        match t with 
        | Variable (_) -> true 
        | _ -> false
  
    let index2Name fi (ctx : Context) x =
        try 
            let (xn, _) = List.item x ctx 
            xn
        with
        | Failure _ ->
          let msg =
            Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
          error fi (msg x (List.length ctx))

    let rec printtmTerm outer ctx t =
        match t with
        | Abstraction (_, x, t2) ->
            let (ctx', x') = pickfreshname ctx x
            pr "lambda "
            pr x'
            pr "."
            if (small t2) && (not outer) then printSpace ()
            printtmTerm outer ctx' t2;
        | t -> printApplicationTerm outer ctx t
    and printApplicationTerm outer ctx t =
        match t with
        | Application (_, t1, t2) ->
            printApplicationTerm false ctx t1
            printSpace ()
            printTerm false ctx t2
        | t -> printTerm outer ctx t
    and printTerm outer (ctx : Context) t =
        match t with
        | Variable (fi, x, n) ->
            match fi with
            | FI (_, 1, _) ->
                // bottom variable always uniquely bound, so de Bruijn indexing can be safely ingored
                pr (index2Name fi ctx x)
            | _ ->
                if (ctxLength ctx) = n
                then pr (index2Name fi ctx x)
                else
                    pr
                      ("[bad index: " +
                         ((string x) +
                            ("/" +
                               ((string n) +
                                  (" in {" +
                                     ((List.fold (fun s (x, _) -> s + (" " + x)) ""
                                         ctx)
                                        + " }]"))))))
        | t -> (pr "("; printtmTerm outer ctx t; pr ")")
  
    let printtm ctx t = printtmTerm true ctx t
  
    let printBinding ctx b =
        match b with 
        | NameBind -> () 
        | AbstractionBind t -> (pr "= "; printtm ctx t)
  