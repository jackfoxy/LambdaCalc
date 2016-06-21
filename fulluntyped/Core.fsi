/// Core evaluation 
module Core

open Ast

/// Evaluation
val eval : ctx : Context -> t : Term -> Term

/// Evaluate binding
val evalBinding : ctx : Context ->  b : Binding -> Binding
