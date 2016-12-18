namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Ast
open Jackfoxy.LambdaCalc
open CommandLine

module Reduce =

    val processInput : inputSource : Source list -> (string * Binding) list 
