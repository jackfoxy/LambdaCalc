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

module Compatability =

    val setOutput : parsedCommand : ParsedCommand -> unit

    val pr: string -> unit

    val open_hvbox : indent : int -> unit

    val open_hovbox : indent : int -> unit

    val print_int : i : int -> unit

    val close_box : unit -> unit

    val print_break : int -> int -> unit

    val print_space : unit -> unit

    val force_newline : unit -> unit

    val print_flush : unit -> unit