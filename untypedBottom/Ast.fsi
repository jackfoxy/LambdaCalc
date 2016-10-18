
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

    (λx.t12) v2 -> [x ↦ v2]t12    /* abstraction value */

*)

type Term =
    | Variable of FileInfo : Info * DeBruinIndex : int * ContextLength : int
    | Abstraction of FileInfo : Info * Name : string * Abstraction: Term
    | Application of FileInfo : Info * Applicator : Term * Applicand : Term
//    /// Bottom = λ t. λ b. b
//    | Bottom of FileInfo : Info * Name : string * Info * string * Info * int * int
//    /// Fix = λ f. (λ x. f (λ y. x x y))(λ x. f (λ y. x x y))
//    | Fix of FileInfo : Info * Name : string *          //Abstraction f
//                (Info *                                     //Application 
//
//                    (Info * string *                            //Abstraction x
//                        (Info *                                     //Application
//                            (Info * int * int) 
//                            *
//                            (Info * string *                            //Abstraction y
//                                (Info *                                     //Application
//                                    (Info *                                     //Application
//                                        (Info *                                     //Application 
//                                            (Info * int * int) * (Info * int * int) 
//                                        )
//                                        * 
//                                        (Info * int * int) 
//                                    )
//                                    *
//                                    (Info * int * int)
//                                )
//                            )
//                        )
//                    )
//                    *
//                    (Info * string *                            //Abstraction x
//                        (Info *                                     //Application
//                            (Info * int * int) 
//                            *
//                            (Info * string *                            //Abstraction y
//                                (Info *                                     //Application
//                                    (Info *                                     //Application
//                                        (Info *                                     //Application 
//                                            (Info * int * int) * (Info * int * int) 
//                                        )
//                                        * 
//                                        (Info * int * int) 
//                                    )
//                                    *
//                                    (Info * int * int)
//                                )
//                            )
//                        )
//                    )
//                )

type Binding = 
    | NameBind 
    | AbbstractionBind of Term

type Command =
    | Eval of FileInfo : Info * Term 
    | Bind of FileInfo : Info * Name : string * Binding

val termInfo : t : Term -> Info

type Context = (string * Binding) list

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
