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

    val reportError : ParsedCommand -> unit

    val fileNameFromPaths : paths : string list -> string

    val inputReader : paths : string list -> StringReader

module List =
    val assoc : a : 'a  -> l : ('a * 'b) list -> 'b option when 'a : equality

module Common =

    exception NoRuleAppliesException
    exception NotFoundException

open CommandLine

module PrettyPrint =

    val mutable useLambda : bool

    val pr: string -> unit

    val printInt : i : int -> unit

    val printBreak : unit -> unit

    val printSpace : unit -> unit

    val forceNewline : unit -> unit