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

let rec isval ctx t =
  match t with
  | TmAbs (_) -> true
  | _ -> false
  
let rec eval1 ctx t =
  match t with
  | TmVar (fi, n, _) ->
      (match getBinding fi ctx n with
       | TmAbbBind t -> t
       | _ -> raise Common.NoRuleAppliesException)
  | TmApp (_, (TmAbs (_, _, t12)), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) -> let t1' = eval1 ctx t1 in TmApp (fi, t1', t2)
  | _ -> raise Common.NoRuleAppliesException
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | Common.NoRuleAppliesException -> t
  
let evalBinding ctx b =
  match b with
  | TmAbbBind t -> let t' = eval ctx t in TmAbbBind t'
  | bind -> bind
