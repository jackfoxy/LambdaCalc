namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Jackfoxy.LambdaCalc

/// Evaluation 
module Core =

    /// Evaluation
    val eval : ctx : Context -> t : Term -> Term
