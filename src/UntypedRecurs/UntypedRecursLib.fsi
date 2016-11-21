namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Ast
open Jackfoxy.LambdaCalc.CommandLine

module UntypedRecursLib =

    val processInput : parsedCommand : ParsedCommand -> input : Source -> ctx : Context  -> (string * Binding) list 
