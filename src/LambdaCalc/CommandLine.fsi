namespace Jackfoxy.LambdaCalc

open System.IO

/// Command line parsing.
module CommandLine = 
    
    /// lambda calculus input
    type Source =
        /// file path of lambda calculus input
        | File of string list
        /// console lambda calculus input
        | Console of string
        /// no input
        | NoSource

    /// output target - not implemented
    type Target =
        | File of string
        | Console

    /// console command parsed
    type ParsedCommand =
        {
            /// command line usage
            Usage : string
            /// source files and console input
            Source : Source list
            /// not implemented
            Target : Target
            /// use the UTF-8 encoding lambda character
            Lambda : bool
            /// error message on failure
            ErrorMsg: string option
        }

    val parse : programName : string -> argv : string [] -> ParsedCommand
