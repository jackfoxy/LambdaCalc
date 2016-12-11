namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Jackfoxy.LambdaCalc
open CommonAst
open Ast

/// Evaluation 
module Core =

    /// Evaluation
    val eval : ctx : Context -> t : Term -> Term

    /// Evaluate binding
    val evalBinding : ctx : Context ->  b : Binding -> Binding
