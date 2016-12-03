namespace Jackfoxy.LambdaCalc.Untyped

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

open Support.Error
open Jackfoxy.LambdaCalc.PrettyPrint

type Term =
    | Variable of FileInfo : Info * DeBruinIndex : int * ContextLength : int
    | Abstraction of FileInfo : Info * Name : string * Abstraction: Term
    | Application of FileInfo : Info * Applicator : Term * Applicand : Term

type Binding = 
    | NameBind 
    | AbbstractionBind of Term

type Context = (string * Binding) list

type Command = 
    | Eval of FileInfo : Info * Term 
    | Bind of FileInfo : Info * Name : string * Binding

/// Syntax trees and associated support functions.
module Ast =

    (* Context management *)
    let emptyContext : Context = []
  
    let ctxLength (ctx : Context) = List.length ctx
  
    let addBinding (ctx : Context) x bind = (x, bind) :: ctx
  
    let addName ctx x = addBinding ctx x NameBind
  
    let rec isName (ctx : Context) x =
        match ctx with
        | [] -> false
        | (y, _) :: rest -> if y = x then true else isName rest x
  
    let rec pickfreshname ctx x =
        if isName ctx x
        then pickfreshname ctx (x + "'")
        else (((x, NameBind) :: ctx), x)
  
    let rec name2Index fi (ctx : Context) x =
        match ctx with
        | [] -> error fi ("Identifier " + (x + " is unbound"))
        | (y, _) :: rest -> if y = x then 0 else 1 + (name2Index fi rest x)
  
    (* Shifting *)
    let tmmap onvar c t =
        let rec walk c t =
            match t with
            | Variable (fi, x, n) -> 
                onvar fi c x n
            | Abstraction (fi, x, t2) -> 
                Abstraction (fi, x, walk (c + 1) t2)
            | Application (fi, t1, t2) -> 
                Application (fi, walk c t1, walk c t2)
        walk c t
  
    let termShiftAbove d c t =
        tmmap
            (fun fi c x n ->
                if x >= c then Variable (fi, x + d, n + d) else Variable (fi, x, n + d))
            c t
  
    let termShift d t = termShiftAbove d 0 t
  
    let bindingshift d bind =
        match bind with
        | NameBind -> NameBind
        | AbbstractionBind t -> AbbstractionBind (termShift d t)
  
    (* Substitution *)
    let termSubst j s t =
        tmmap
            (fun fi c x n -> if x = (j + c) then termShift c s else Variable (fi, x, n))
            0 t
  
    let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
  
    (* Context management (continued) *)
    let rec getBinding fi (ctx : Context) i =
        try 
            let (_, bind) = List.item i ctx 
            bindingshift (i + 1) bind
        with
        | Failure _ ->
          let msg =
            Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
          error fi (msg i (List.length ctx))
  
    (* Extracting file info *)
    let termInfo t =
        match t with
        | Variable (fi, _, _) -> fi
        | Abstraction (fi, _, _) -> fi
        | Application (fi, _, _) -> fi
  
    (* Printing *)
    (* The printing functions call these utility functions to insert grouping
      information and line-breaking hints for the pretty-printing library:
         obox   Open a "box" whose contents will be indented by two spaces if
                the whole box cannot fit on the current line
         obox0  Same but indent continuation lines to the same column as the
                beginning of the box rather than 2 more columns to the right
         cbox   Close the current box
         break  Insert a breakpoint indicating where the line maybe broken if
                necessary.
      See the documentation for the Format module in the OCaml library for
      more details. 
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
            if (small t2) && (not outer) then printSpace () //then printBreak () else printSpace ()
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
        | AbbstractionBind t -> (pr "= "; printtm ctx t)
  