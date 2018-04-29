namespace Jackfoxy.LambdaCalc

open CommandLine
open System.IO

/// Common evaluation routines.
module Common =

    type InputLines = 
        {
        Input : string
        Lines : string []
        PriorLineCount : int
        }

    type Input =
        {
        InputReader : StringReader
        ConcatNames : string
        InputLines : InputLines list
        }

    type CommentLine =
        {
        LineNbr : int
        Comment : string
        }

    val reportError : ParsedCommand -> unit

    val getInput : internalInput : string option -> paths : string list -> secondaryInput : string option -> Input

    val getCommentLines : inputLines : InputLines list -> CommentLine list
