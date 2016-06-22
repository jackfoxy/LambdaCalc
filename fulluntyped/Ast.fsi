
/// Syntax trees and associated support functions.
module Ast

open Support.Error

type Term =
    | TmVar of Info * int * int
    | TmAbs of Info * string * Term
    | TmApp of Info * Term * Term

type Binding = 
    | NameBind 
    | TmAbbBind of Term

type Command =
    | Eval of Info * Term 
    | Bind of Info * string * Binding

val tmInfo : t : Term -> Info

type Context = (string * Binding) list

val emptyContext : Context

val ctxLength : ctx : Context -> int

val name2Index : fi : Info -> ctx : Context -> x : string-> int

val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

val addName : ctx : Context -> x : string -> Context

val termSubstTop : s : Term -> t : Term -> Term

val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

val prBinding : ctx : Context -> b : Binding -> unit
