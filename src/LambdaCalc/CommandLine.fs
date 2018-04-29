namespace Jackfoxy.LambdaCalc

open Argu
open System.IO

module CommandLine = 
    let bind f = 
        function
        | Ok x -> f x
        | Error x -> Error x

    let returnM = Ok

    type EitherBuilder() =
        member __.Return a = returnM a
        member __.Bind (m, f) = bind f m
        member __.ReturnFrom m = m

    let choose = EitherBuilder()

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
        | [<MainCommand; AppSettingsSeparator(',')>] InputPaths of string list
        //| [<AltCommandLine("-o")>] Output of string
        | [<AltCommandLine("-c"); Unique>] ConsoleInput of string
        
        | [<AltCommandLine("-i"); Unique>] InputFolder of string
        | [<AltCommandLine("-l"); Unique>] Lambda
  
         with
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | InputPaths _ -> "(optional) list file paths (space separated) of input in order to process"
                    //| Output _ -> "(optional, not implemented) output path"
                    | ConsoleInput _ -> "input from console for process after optional files"
                    | Lambda -> @"use and print \u03BB for lambda followed by space"
                    | InputFolder _ -> "folder for input paths"

    let parseCommandLine programName (argv : string []) = 

        try
            match argv, argv.Length with
            | _, 0 -> 
                Error "no arguments"
            | help, 1  when help.[0].ToLower() = "--help" ->
                Error ""
            | _, _ ->
                let parser = 
                    ArgumentParser.Create<CLIArguments>(programName = programName)

                let commandLine = parser.Parse argv
                let usage = parser.PrintUsage()

                Ok (commandLine, usage)
        with e ->
            match e with
            | :? System.ArgumentException -> Error "unrecognized arguments"
            | _ -> Error e.Message
             
    let parseTarget (commandLine : ParseResults<CLIArguments>) = 

        let targetList = 
            []
//            |> (fun x -> 
//                    if commandLine.Contains Output then 
//                        match commandLine.TryGetResult Output with
//                        | Some path -> (Target.File path)::x
//                        | None -> x
//                    else x)

        match targetList with
        | [] -> Ok Target.Console
        | [x] -> Ok x
        | hd::tl -> Error (sprintf "more than one output target specified: %s, %s" (hd.ToString()) (tl.Head.ToString()))

    let fileInput (commandLine : ParseResults<CLIArguments>) fileList =
        let inputs =
            match (commandLine.TryGetResult InputFolder) with
            | Some x -> 

                fileList
                |> List.map (fun p -> Path.Combine( [|x; p|]))

            | None -> fileList

        match inputs
            |> List.tryFind (fun x -> x |> (File.Exists >> not)) with
        | Some x ->
            sprintf "input file does not exist: %s" x
            |> Error 
        | None ->
            Source.File inputs   
            |> Ok 
        
    let parseSource (commandLine : ParseResults<CLIArguments>) = 

        let sourceList = 
            []
            |> (fun x -> 
                    match commandLine.TryGetResult InputPaths with
                    | Some paths -> 
                        (Source.File paths)::x
                    | None -> x
                    )
            |> (fun x -> 
                    match commandLine.TryGetResult ConsoleInput with
                    | Some consoleInput -> 
                        (Source.Console consoleInput)::x
                    | None -> x
                    )

        sourceList
        |> List.fold (fun state t -> 
            match state with
            | Ok source ->
                match t with
                | Source.File fileList ->
                    match fileInput commandLine fileList with
                    | Ok x -> 
                        Ok (x::source)
                    | Error x ->
                        Error x
                | x -> 
                    Ok (x::source)
            | Error x ->
                Error x
            ) (Ok [])


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
                                Lambda = commandLine.Contains Lambda
                                ErrorMsg = None
                            } 
                        } with
        | Ok x -> x
        | Error msg -> 
            let usage = ArgumentParser.Create<CLIArguments>(programName = programName).PrintUsage()
            {
                Usage = usage
                Source = []
                Target = Target.Console 
                Lambda = false
                ErrorMsg = Some msg
            } 
