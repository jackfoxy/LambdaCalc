(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Syntax trees and associated support functions.
module Ast

open Support.Error
open FSharpTapl.Compatability

(* ---------------------------------------------------------------------- *)
(* Datatypes *)
type Term =
  | TmTrue of Info
  | TmFalse of Info
  | TmIf of Info * Term * Term * Term
  | TmVar of Info * int * int
  | TmAbs of Info * string * Term
  | TmApp of Info * Term * Term
  | TmRecord of Info * (string * Term) list
  | TmProj of Info * Term * string
  | TmFloat of Info * float
  | TmTimesfloat of Info * Term * Term
  | TmString of Info * string
  | TmZero of Info
  | TmSucc of Info * Term
  | TmPred of Info * Term
  | TmIsZero of Info * Term
  | TmLet of Info * string * Term * Term

type Binding = 
    | NameBind 
    | TmAbbBind of Term

type Context = (string * Binding) list

type Command = 
    | Eval of Info * Term 
    | Bind of Info * string * Binding

(* ---------------------------------------------------------------------- *)
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
  then pickfreshname ctx (x ^ "'")
  else (((x, NameBind) :: ctx), x)
  
let index2Name fi (ctx : Context) x =
  try let (xn, _) = List.item x ctx in xn
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg x (List.length ctx))
  
let rec name2Index fi (ctx : Context) x =
  match ctx with
  | [] -> error fi ("Identifier " ^ (x ^ " is unbound"))
  | (y, _) :: rest -> if y = x then 0 else 1 + (name2Index fi rest x)
  
(* ---------------------------------------------------------------------- *)
(* Shifting *)
let tmmap onvar c t =
  let rec walk c t =
    match t with
    | (TmTrue _ as t) -> t
    | (TmFalse _ as t) -> t
    | TmIf (fi, t1, t2, t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, t2) -> TmAbs (fi, x, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmProj (fi, t1, l) -> TmProj (fi, walk c t1, l)
    | TmRecord (fi, fields) ->
        TmRecord (fi, List.map (fun (li, ti) -> (li, (walk c ti))) fields)
    | (TmFloat _ as t) -> t
    | TmTimesfloat (fi, t1, t2) -> TmTimesfloat (fi, walk c t1, walk c t2)
    | (TmString _ as t) -> t
    | TmZero fi -> TmZero fi
    | TmSucc (fi, t1) -> TmSucc (fi, walk c t1)
    | TmPred (fi, t1) -> TmPred (fi, walk c t1)
    | TmIsZero (fi, t1) -> TmIsZero (fi, walk c t1)
    | TmLet (fi, x, t1, t2) -> TmLet (fi, x, walk c t1, walk (c + 1) t2)
  in walk c t
  
let termShiftAbove d c t =
  tmmap
    (fun fi c x n ->
       if x >= c then TmVar (fi, x + d, n + d) else TmVar (fi, x, n + d))
    c t
  
let termShift d t = termShiftAbove d 0 t
  
let bindingshift d bind =
  match bind with
  | NameBind -> NameBind
  | TmAbbBind t -> TmAbbBind (termShift d t)
  
(* ---------------------------------------------------------------------- *)
(* Substitution *)
let termSubst j s t =
  tmmap
    (fun fi c x n -> if x = (j + c) then termShift c s else TmVar (fi, x, n))
    0 t
  
let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
  
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)
let rec getBinding fi (ctx : Context) i =
  try let (_, bind) = List.item i ctx in bindingshift (i + 1) bind
  with
  | Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in error fi (msg i (List.length ctx))
  
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)
let tmInfo t =
  match t with
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmProj (fi, _, _) -> fi
  | TmRecord (fi, _) -> fi
  | TmFloat (fi, _) -> fi
  | TmTimesfloat (fi, _, _) -> fi
  | TmString (fi, _) -> fi
  | TmZero fi -> fi
  | TmSucc (fi, _) -> fi
  | TmPred (fi, _) -> fi
  | TmIsZero (fi, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  
(* ---------------------------------------------------------------------- *)
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
let obox0 () = open_hvbox 0
let obox () = open_hvbox 2
let cbox () = close_box()
let ``break`` () = print_break 0 0
  
let small t = match t with | TmVar (_) -> true | _ -> false
  
let rec printtmTerm outer ctx t =
  match t with
  | TmIf (_, t1, t2, t3) ->
      (obox0 ();
       pr "if ";
       printtmTerm false ctx t1;
       print_space ();
       pr "then ";
       printtmTerm false ctx t2;
       print_space ();
       pr "else ";
       printtmTerm false ctx t3;
       cbox ())
  | TmAbs (_, x, t2) ->
      let (ctx', x') = pickfreshname ctx x
      in
        (obox ();
         pr "lambda ";
         pr x';
         pr ".";
         if (small t2) && (not outer) then ``break`` () else print_space ()
         printtmTerm outer ctx' t2;
         cbox ())
  | TmLet (_, x, t1, t2) ->
      (obox0 ();
       pr "let ";
       pr x;
       pr " = ";
       printtmTerm false ctx t1;
       print_space ();
       pr "in";
       print_space ();
       printtmTerm false (addName ctx x) t2;
       cbox ())
  | t -> printtmAppTerm outer ctx t
and printtmAppTerm outer ctx t =
  match t with
  | TmApp (_, t1, t2) ->
      (obox0 ();
       printtmAppTerm false ctx t1;
       print_space ();
       printtmATerm false ctx t2;
       cbox ())
  | TmTimesfloat (_, _, t2) ->
      (pr "timesfloat ";
       printtmATerm false ctx t2;
       pr " ";
       printtmATerm false ctx t2)
  | TmPred (_, t1) -> (pr "pred "; printtmATerm false ctx t1)
  | TmIsZero (_, t1) -> (pr "iszero "; printtmATerm false ctx t1)
  | t -> printtmPathTerm outer ctx t
and printtmPathTerm outer ctx t =
  match t with
  | TmProj (_, t1, l) -> (printtmATerm false ctx t1; pr "."; pr l)
  | t -> printtmATerm outer ctx t
and printtmATerm outer (ctx : Context) t =
  match t with
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | TmVar (fi, x, n) ->
      if (ctxLength ctx) = n
      then pr (index2Name fi ctx x)
      else
        pr
          ("[bad index: " ^
             ((string x) ^
                ("/" ^
                   ((string n) ^
                      (" in {" ^
                         ((List.fold (fun s (x, _) -> s ^ (" " ^ x)) ""
                             ctx)
                            ^ " }]"))))))
  | TmRecord (_, fields) ->
      let pf i (li, ti) =
        (if li <> (string i) then (pr li; pr "=") else ();
         printtmTerm false ctx ti) in
      let rec p i l =
        (match l with
         | [] -> ()
         | [ f ] -> pf i f
         | f :: rest ->
             (pf i f;
              pr ", ";
              if not outer then ``break`` () //if outer then print_space () else break ();
              p (i + 1) rest))
      in (pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox ())
  | TmFloat (_, s) -> pr (string s)
  | TmString (_, s) -> pr ("\"" ^ (s ^ "\""))
  | TmZero _ -> pr "0"
  | TmSucc (_, t1) ->
      let rec f n t =
        (match t with
         | TmZero _ -> pr (string n)
         | TmSucc (_, s) -> f (n + 1) s
         | _ -> (pr "(succ "; printtmATerm false ctx t1; pr ")"))
      in f 1 t1
  | t -> (pr "("; printtmTerm outer ctx t; pr ")")
  
let printtm ctx t = printtmTerm true ctx t
  
let prBinding ctx b =
  match b with 
  | NameBind -> () 
  | TmAbbBind t -> (pr "= "; printtm ctx t)
  

