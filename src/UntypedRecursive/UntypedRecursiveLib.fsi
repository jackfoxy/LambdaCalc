namespace Jackfoxy.LambdaCalc.UntypedRecursive

open Ast
open Jackfoxy.LambdaCalc.CommandLine

module UntypedBottomLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  -> (string * Binding) list 
