(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

/// Core typechecking and evaluation functions.
module Core

open Ast
open FSharpTapl
open Support.Error

(* ------------------------   EVALUATION  ------------------------ *)
  
let rec isnumericval ctx t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval ctx t1
  | _ -> false
  
let rec isval ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmFloat _ -> true
  | TmString _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs (_) -> true
  | TmRecord (_, fields) -> List.forall (fun (_, ti) -> isval ctx ti) fields
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmIf (_, (TmTrue _), t2,_) -> t2
  | TmIf (_, (TmFalse _), _, t3) -> t3
  | TmIf (fi, t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (fi, t1', t2, t3)
  | TmVar (fi, n, _) ->
      (match getBinding fi ctx n with
       | TmAbbBind t -> t
       | _ -> raise Common.NoRuleAppliesException)
  | TmApp (_, (TmAbs (_, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | TmRecord (fi, fields) ->
      let rec evalafield l =
        (match l with
         | [] -> raise Common.NoRuleAppliesException
         | (l, vi) :: rest when isval ctx vi ->
             let rest' = evalafield rest in (l, vi) :: rest'
         | (l, ti) :: rest -> let ti' = eval1 ctx ti in (l, ti') :: rest) in
      let fields' = evalafield fields in TmRecord (fi, fields')
  | TmProj (_, ((TmRecord (_, fields) as v1)), l) when isval ctx v1 ->
      match List.assoc l fields with 
      | Some x -> x
      | None -> raise Common.NoRuleAppliesException
  | TmProj (fi, t1, l) -> let t1' = eval1 ctx t1 in TmProj (fi, t1', l)
  | TmTimesfloat (fi, (TmFloat (_, f1)), (TmFloat (_, f2))) ->
      TmFloat (fi, f1 * f2)
  | TmTimesfloat (fi, ((TmFloat (_) as t1)), t2) ->
      let t2' = eval1 ctx t2 in TmTimesfloat (fi, t1, t2')
  | TmTimesfloat (fi, t1, t2) ->
      let t1' = eval1 ctx t1 in TmTimesfloat (fi, t1', t2)
  | TmSucc (fi, t1) -> let t1' = eval1 ctx t1 in TmSucc (fi, t1')
  | TmPred (_, (TmZero _)) -> TmZero dummyinfo
  | TmPred (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 -> nv1
  | TmPred (fi, t1) -> let t1' = eval1 ctx t1 in TmPred (fi, t1')
  | TmIsZero (_, (TmZero _)) -> TmTrue dummyinfo
  | TmIsZero (_, (TmSucc (_, nv1))) when isnumericval ctx nv1 ->
      TmFalse dummyinfo
  | TmIsZero (fi, t1) -> let t1' = eval1 ctx t1 in TmIsZero (fi, t1')
  | TmLet (_, _, v1, t2) when isval ctx v1 -> termSubstTop v1 t2
  | TmLet (fi, x, t1, t2) -> let t1' = eval1 ctx t1 in TmLet (fi, x, t1', t2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
let evalBinding ctx b =
  match b with
  | TmAbbBind t -> let t' = eval ctx t in TmAbbBind t'
  | bind -> bind
  

