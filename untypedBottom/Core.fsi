/// Core evaluation 
module Core

open Ast

type ApplicationInfo =
    {
    ParentAbstractionName : string
    ParentAbstraction : Term
    ApplicatorName : string
    Applicator : Term
    ApplicandName : string option
    Applicand : Term
    ApplicandIsApplication : bool
    }

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

/// Evaluate binding
val evalBinding : ctx : Context ->  b : Binding -> Binding
