namespace Jackfoxy.LambdaCalc.Untyped

open Jackfoxy.LambdaCalc
open Ast

/// Evaluation 
module Core =

    /// Evaluation
    val eval : ctx : Context -> t : Term -> Term

    /// Evaluate binding
    val evalBinding : ctx : Context ->  b : Binding -> Binding
