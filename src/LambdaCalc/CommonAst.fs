namespace Jackfoxy.LambdaCalc

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)
open Support.Error

type Term =
    | Variable of FileInfo : Info * DeBruinIndex : int * ContextLength : int
    | Abstraction of FileInfo : Info * Name : string * Abstraction: Term
    | Application of FileInfo : Info * Applicator : Term * Applicand : Term

type Binding = 
    | NameBind 
    | AbstractionBind of Term

type Context = (string * Binding) list

type Command = 
    | Eval of FileInfo : Info * Term 
    | Bind of FileInfo : Info * Name : string * Binding

module CommonAst =
    let emptyContext : Context = []

    let ctxLength (ctx : Context) = List.length ctx
  
    let addBinding (ctx : Context) x binding = (x, binding) :: ctx
  
    let addName ctx x = addBinding ctx x NameBind
  
    let rec isName (ctx : Context) x =
        match ctx with
        | [] -> false
        | (y, _) :: rest -> if y = x then true else isName rest x
  
    let rec pickfreshname ctx x =
        if isName ctx x
        then pickfreshname ctx (x + "'")
        else (((x, NameBind) :: ctx), x)
  
    let rec name2Index fileInfo (ctx : Context) x =
        match ctx with
        | [] -> error fileInfo ("Identifier '" + (x + "' is unbound"))
        | (y, _) :: rest -> if y = x then 0 else 1 + (name2Index fileInfo rest x)

    (* Shifting *)
    let tmmap onvar c term =
        let rec walk c term =
            match term with
            | Variable (fileInfo, x, n) -> 
                onvar fileInfo c x n
            | Abstraction (fileInfo, x, t2) -> 
                Abstraction (fileInfo, x, walk (c + 1) t2)
            | Application (fileInfo, t1, t2) -> 
                Application (fileInfo, walk c t1, walk c t2)
        walk c term
  
    let termShiftAbove d c term =
        tmmap
            (fun fileInfo c x n ->
                if x >= c then Variable (fileInfo, x + d, n + d) else Variable (fileInfo, x, n + d))
            c term
  
    let termShift d term = termShiftAbove d 0 term
  
    let bindingshift d bind =
        match bind with
        | NameBind -> NameBind
        | AbstractionBind t -> AbstractionBind (termShift d t)

    (* Substitution *)
    let termSubst j s term =
        tmmap
            (fun fileInfo c x n -> if x = (j + c) then termShift c s else Variable (fileInfo, x, n))
            0 term
  
    let termSubstTop s term = termShift (-1) (termSubst 0 (termShift 1 s) term)

    (* Context management (continued) *)
    let rec getBinding fileInfo (ctx : Context) i =
        try 
            let (_, bind) = List.item i ctx 
            bindingshift (i + 1) bind
        with
        | Failure _ ->
          let msg =
            Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
          error fileInfo (msg i (List.length ctx))
  
    (* Extracting file info *)
    let termInfo term =
        match term with
        | Variable (fileInfo, _, _) -> fileInfo
        | Abstraction (fileInfo, _, _) -> fileInfo
        | Application (fileInfo, _, _) -> fileInfo