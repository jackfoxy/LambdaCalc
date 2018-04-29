namespace Jackfoxy.LambdaCalc.Untyped

open Jackfoxy.LambdaCalc

/// Evaluation 
module Core =

    /// Evaluation
    val eval : ctx : Context -> term : Term -> Term
