namespace Jackfoxy.LambdaCalc

open System.IO

module CommandLine = 
    
    type Source =
        | File of string list
        | Console of string
        | NoSource

    type Target =
        | File of string
        | Console

    type ParsedCommand =
        {
        Usage : string
        Source : Source
        Target : Target
        Lambda : bool
        ErrorMsg: string option
        }

    val parse : programName : string -> argv : string [] -> ParsedCommand
