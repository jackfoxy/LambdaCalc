namespace Jackfoxy.LambdaCalc.UntypedRecursive

open Ast

/// Core evaluation 
module Core =

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
