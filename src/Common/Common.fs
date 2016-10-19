namespace FSharpTapl

open Argu
open FSharpx.Choice

module CommandLine = 

    let (|Success|Failure|) = function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

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

    type CLIArguments =
        | [<AltCommandLine("-i")>] Input of string
        | [<AltCommandLine("-o")>] Output of string
        | [<AltCommandLine("-s")>] ConsoleInput of string 
        | [<AltCommandLine("-l")>] Lambda
  
         with
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | Input _ -> "(optional) file path to process"
                    | Output _ -> "(optional, not implemented) output path"
                    | ConsoleInput _ -> "input from console"
                    | Lambda -> @"use and print \u03BB for lambda followed by space"

    let parseCommandLine argv = 

        try
            Success (ArgumentParser.Create<CLIArguments>().Parse argv)
        with e ->
            match e with
            | :? System.ArgumentException -> Failure e.Message
            | _ -> raise e
             
    let parseTarget (commandLine : ParseResults<CLIArguments>) = 

        let targetList = 
            []
            |> (fun x -> 
                    if commandLine.Contains <@ Output @> then 
                        match commandLine.TryGetResult <@ Output @> with
                        | Some path -> (Target.File path)::x
                        | None -> x
                    else x)

        match targetList with
        | [] -> Success Target.Console
        | [x] -> Success x
        | hd::tl -> Failure (sprintf "more than one output target specified: %s, %s" (hd.ToString()) (tl.Head.ToString()))

    let parseSource (commandLine : ParseResults<CLIArguments>) = 

        let sourceList = 
            []
            |> (fun x -> 
                    if commandLine.Contains <@ Input @> then 
                        match commandLine.TryGetResult <@ Input @> with
                        | Some path -> (Source.File path)::x
                        | None -> x
                        
                    else x)
            |> (fun x -> 
                    if commandLine.Contains <@ ConsoleInput @> then 
                        match commandLine.TryGetResult <@ ConsoleInput @> with
                        | Some consoleInput -> (Source.Console consoleInput)::x
                        | None -> x
                    else x)

        match sourceList with
        | [] -> Success Source.NoSource
        | [x] -> Success x
        | hd::tl ->Failure (sprintf "more than one input source specified: %s, %s" (hd.ToString()) (tl.Head.ToString()))
        
    let parse argv = 

        match choose { 
                        let! commandLine = parseCommandLine argv
                       
                        let! target = parseTarget commandLine
                        let! source = parseSource commandLine

                        return 
                            {
                            Usage = commandLine.Usage()
                            Source = source
                            Target = target
                            Lambda = commandLine.Contains <@ Lambda @>
                            ErrorMsg = None
                            } 
                        } with
        | Success x -> x
        | Failure msg -> 
            let commandLine = ArgumentParser.Create<CLIArguments>()
            {
            Usage = commandLine.Usage()
            Source = Source.NoSource
            Target = Target.Console 
            Lambda = false
            ErrorMsg = Some msg
            } 

    let reportError (parsedCommand : ParsedCommand) =
        
        match parsedCommand.ErrorMsg with
        | Some msg ->
            printfn "%s" msg
            printfn " "
        | None -> ()

        printfn "%s" parsedCommand.Usage

module List =
    let assoc a (l : ('a * 'b) list) =
        
        let rec loop (l' : ('a * 'b) list) =
            match l' with
            | [] -> None
            | (a', b')::tl ->
                if a' = a then Some b'
                else loop tl
        loop l

open FSharp.Compatibility.OCaml.Format

open CommandLine
open System.IO

module Compatability =

    let mutable private output' : System.IO.TextWriter option = None

    let mutable private formatter' : formatter option = None

    let mutable private parsedCommand : ParsedCommand =
        {
        Usage = ""
        Source = Source.NoSource
        Target = Target.Console 
        Lambda = false
        ErrorMsg = None
        } 

    let private makeOutput (parsedCommand' : ParsedCommand) =

        parsedCommand <- parsedCommand'

        output' <-
            match parsedCommand.Target with
            | Target.File path -> 
                new StreamWriter(path) :> TextWriter
                |> TextWriter.Synchronized
                |> Some 
            | Target.Console -> Some Microsoft.FSharp.Core.Operators.stdout 

        formatter' <- (output'.Value |> formatter_of_out_channel |> Some )

    let setOutput (parsedCommand : ParsedCommand) =

        match output' with
        | None ->
            makeOutput parsedCommand
            
        | Some x -> 
            x.Dispose()
            makeOutput parsedCommand

    let getOutputFormatter() =
        
        match output' with
        | None ->
            invalidArg "" ""
        | Some _ -> 
            formatter'.Value

    let pr (s : string) = 
        let s' = if parsedCommand.Lambda then s.Replace("lambda ", "\u03BB") else s
        (getOutputFormatter()
        |> pp_print_string) s'

    let open_hovbox indent =
        (getOutputFormatter()
        |> pp_open_hovbox) indent
        
    let open_hvbox indent =
        (getOutputFormatter()
        |> pp_open_hvbox) indent

    let print_int i = 
        (getOutputFormatter()
        |> pp_print_int) i

    let close_box() =
        (getOutputFormatter()
        |> pp_close_box) ()

    let print_break width offset =
        (getOutputFormatter()
        |> pp_print_break) width offset

    let print_space() =
        (getOutputFormatter()
        |> pp_print_space) ()

    let force_newline() =
        (getOutputFormatter()
        |> pp_force_newline) ()

    let print_flush () =
        let formatter = getOutputFormatter()
        pp_print_flush formatter ()

module Common =

    exception NoRuleAppliesException
    exception NotFoundException
