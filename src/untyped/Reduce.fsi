namespace Jackfoxy.LambdaCalc.Untyped

open Ast
open Jackfoxy.LambdaCalc
open CommandLine

module Reduce =

    val processInput : inputSource : Source -> (string * Binding) list 
