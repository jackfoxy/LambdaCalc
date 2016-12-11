namespace Jackfoxy.LambdaCalc.Untyped

open Ast
open Jackfoxy.LambdaCalc
open CommandLine

module UntypedLib =

    val processInput : inputSource : Source -> (string * Binding) list 
