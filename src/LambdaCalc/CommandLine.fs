namespace Jackfoxy.LambdaCalc

open Argu
open FSharpx.Choice
open System.IO

module CommandLine = 

    let (|Success|Failure|) = function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

    type Source =
        | File of string list
        | Console of string
        | NoSource
        with
            member __.ErrorToString() =
                match __ with
                | File _ -> "File(s)"
                | Console _ -> "Console"
                | NoSource -> "NoSource"

    type Target =
        | File of string
        | Console

    type ParsedCommand =
        {
        Usage : string
        Source : Source list
        Target : Target
        Lambda : bool
        ErrorMsg: string option
        }

    type CLIArguments =
        | [<MainCommand; Last; AppSettingsSeparator(',')>] InputPaths of string list
        //| [<AltCommandLine("-o")>] Output of string
        | [<AltCommandLine("-c"); Unique>] ConsoleInput of string
        
        | [<AltCommandLine("-i"); Unique>] InputFolder of string
        | [<AltCommandLine("-l"); Unique>] Lambda
  
         with
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | InputPaths _ -> "(optional, last arg) list file paths to process"
                    //| Output _ -> "(optional, not implemented) output path"
                    | ConsoleInput _ -> "input from console"
                    | Lambda -> @"use and print \u03BB for lambda followed by space"
                    | InputFolder _ -> "folder for input paths"

    let parseCommandLine programName (argv : string []) = 

        try
            match argv, argv.Length with
            | _, 0 -> 
                Failure "no arguments"
            | help, 1  when help.[0].ToLower() = "--help" ->
                Failure ""
            | _, _ ->
                let parser = 
                    ArgumentParser.Create<CLIArguments>(programName = programName)

                let commandLine = parser.Parse argv
                let usage = parser.PrintUsage()

                Success (commandLine, usage)
        with e ->
            match e with
            | :? System.ArgumentException -> Failure "unrecognized arguments"
            | _ -> Failure e.Message
             
    let parseTarget (commandLine : ParseResults<CLIArguments>) = 

        let targetList = 
            []
//            |> (fun x -> 
//                    if commandLine.Contains <@ Output @> then 
//                        match commandLine.TryGetResult <@ Output @> with
//                        | Some path -> (Target.File path)::x
//                        | None -> x
//                    else x)

        match targetList with
        | [] -> Success Target.Console
        | [x] -> Success x
        | hd::tl -> Failure (sprintf "more than one output target specified: %s, %s" (hd.ToString()) (tl.Head.ToString()))

    let fileInput (commandLine : ParseResults<CLIArguments>) fileList =
        let inputs =
            match (commandLine.TryGetResult <@ InputFolder @>) with
            | Some x -> 

                fileList
                |> List.map (fun p -> Path.Combine( [|x; p|]))

            | None -> fileList

        match inputs
            |> List.tryFind (fun x -> x |> (File.Exists >> not)) with
        | Some x ->
            sprintf "input file does not exist: %s" x
            |> Failure 
        | None ->
            Source.File inputs   
            |> Success 
        
    let parseSource (commandLine : ParseResults<CLIArguments>) = 

        let sourceList = 
            []
            |> (fun x -> 
                    match commandLine.TryGetResult <@ InputPaths @> with
                    | Some paths -> 
                        (Source.File paths)::x
                    | None -> x
                    )
            |> (fun x -> 
                    match commandLine.TryGetResult <@ ConsoleInput @> with
                    | Some consoleInput -> 
                        (Source.Console consoleInput)::x
                    | None -> x
                    )

        sourceList
        |> List.fold (fun state t -> 
            match state with
            | Success source ->
                match t with
                | Source.File fileList ->
                    match fileInput commandLine fileList with
                    | Success x -> 
                        Success (x::source)
                    | Failure x ->
                        Failure x
                | x -> 
                    Success (x::source)
            | Failure x ->
                Failure x
            ) (Success [])


    let parse programName argv = 

        match choose { 
                        let! commandLine, usage = parseCommandLine programName argv
                       
                        let! target = parseTarget commandLine
                        let! sources = parseSource commandLine

                        return 
                            {
                            Usage = usage
                            Source = sources
                            Target = target
                            Lambda = commandLine.Contains <@ Lambda @>
                            ErrorMsg = None
                            } 
                        } with
        | Success x -> x
        | Failure msg -> 
            let usage = ArgumentParser.Create<CLIArguments>(programName = programName).PrintUsage()
            {
            Usage = usage
            Source = []
            Target = Target.Console 
            Lambda = false
            ErrorMsg = Some msg
            } 
