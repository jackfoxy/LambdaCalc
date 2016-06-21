﻿namespace FSharpTapl

module CommandLine = 
    
    type Source =
        | File of string
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

    val parse : argv : string [] -> ParsedCommand

    val reportError : ParsedCommand -> unit

module List =
    val assoc : a : 'a  -> l : ('a * 'b) list -> 'b option when 'a : equality

module Common =

    exception NoRuleAppliesException
    exception NotFoundException

    val runMain : main : (unit -> unit) -> 'a

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