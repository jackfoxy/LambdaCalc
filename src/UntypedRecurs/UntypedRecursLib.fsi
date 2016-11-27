namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Ast
open Jackfoxy.LambdaCalc.CommandLine

module UntypedRecursLib =

    val processInput :input : Source -> ctx : Context  -> (string * Binding) list 
