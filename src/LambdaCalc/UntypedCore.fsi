namespace Jackfoxy.LambdaCalc.Untyped

open Ast

/// Core evaluation 
module Core =

    /// Evaluation
    val eval : ctx : Context -> t : Term -> Term

    /// Evaluate binding
    val evalBinding : ctx : Context ->  b : Binding -> Binding
