namespace Jackfoxy.LambdaCalc.UntypedRecursive

open Support.Error

type Term =
    | Variable of FileInfo : Info * DeBruinIndex : int * ContextLength : int
    | Abstraction of FileInfo : Info * Name : string * Abstraction: Term
    | Application of FileInfo : Info * Applicator : Term * Applicand : Term

type Binding = 
    | NameBind 
    | AbbstractionBind of Term

type Command =
    | Eval of FileInfo : Info * Term 
    | Bind of FileInfo : Info * Name : string * Binding

type Context = (string * Binding) list

/// Syntax trees and associated support functions.
module Ast =

    val termInfo : t : Term -> Info

    val emptyContext : Context

    val ctxLength : ctx : Context -> int

    val name2Index : fi : Info -> ctx : Context -> x : string-> int

    val addBinding : ctx : Context -> x : string -> bind : Binding -> Context

    val addName : ctx : Context -> x : string -> Context

    val termSubstTop : s : Term -> t : Term -> Term

    val getBinding : fi : Info -> ctx : Context -> i : int -> Binding

    val printTerm : outer : bool -> ctx : Context -> t : Term -> unit

    val printtmTerm : outer : bool -> ctx : Context -> t : Term -> unit

    val printBinding : ctx : Context -> b : Binding -> unit
