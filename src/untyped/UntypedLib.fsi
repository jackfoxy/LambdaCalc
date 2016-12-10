namespace Jackfoxy.LambdaCalc.Untyped

open Ast
open Jackfoxy.LambdaCalc.CommandLine

module UntypedLib =

    val processInput : inputSource : Source -> (string * Binding) list 
