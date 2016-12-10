namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Ast
open Jackfoxy.LambdaCalc.CommandLine

module UntypedRecursLib =

    val processInput : inputSource : Source -> (string * Binding) list 
