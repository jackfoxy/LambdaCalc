
/// Syntax trees and associated support functions.
module Ast

open Support.Error

(*
TAPL p. 72

Syntax

t ::=
    x       /* variable */
    λx.t    /* abstraction */
    t t     /* application */

v ::=
    λx.t    /* abstraction value */

Evaluation

       t1 -> t1'    
    ---------------      /* E-APP1 */
    t1 t2 -> t1' t2

       t2 -> t2'    
    ---------------      /* E-APP2 */
    v1 t2 -> v1 t2'

    (λx.t12) v2 -> [x |-> v2]t12    /* abstraction value */

*)

type Term =
    | TmVariable of Info * int * int
    | TmAbstraction of Info * string * Term
    | TmApplication of Info * Term * Term

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
