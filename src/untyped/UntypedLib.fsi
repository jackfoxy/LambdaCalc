namespace Jackfoxy.LambdaCalc.Untyped

open Ast
open Jackfoxy.LambdaCalc.CommandLine

module UntypedLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  -> (string * Binding) list 
