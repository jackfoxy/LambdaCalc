namespace Jackfoxy.LambdaCalc

open CommandLine
open System.IO

module Common =

    type InputLines = 
        {
        Input : string
        Lines : string []
        }

    type Input =
        {
        InputReader : StringReader
        ConcatNames : string
        InputLines : InputLines list
        }

    exception NoRuleAppliesException
    exception NotFoundException

    val reportError : ParsedCommand -> unit

    val getInput : internalInput : string -> paths : string list -> Input
