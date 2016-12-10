namespace Jackfoxy.LambdaCalc

open CommandLine
open System.IO

module Common =

    exception NoRuleAppliesException
    exception NotFoundException

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

    let reportError (parsedCommand : ParsedCommand) =
        
        match parsedCommand.ErrorMsg with
        | Some msg ->
            printfn "%s" msg
            printfn " "
        | None -> ()

        printfn "%s" parsedCommand.Usage

    let fileNameFromPaths paths =
        paths
        |> List.fold (fun (s : string) t -> 
            if s.Length = 0 then
                t
            else
                s + " " + t) ""

    let createInput (content, (inputLines : InputLines list)) ((reader : StreamReader), path) =
        let x = reader.ReadToEnd()
        reader.Dispose()
            
        (content + x + "\n"), 
            {
            Input = path
            Lines = x.Split '\n'
            PriorLineCount = 
                inputLines
                |> List.fold (fun s t -> s + t.Lines.Length) 0}::inputLines

    let getInput internalInput (paths : string list) =
        let input =
            paths
            |> List.map (fun x -> (new StreamReader(x)), x )
            |> List.fold createInput ("", [])

        {
        InputReader = new System.IO.StringReader((internalInput + (fst input)).Replace("\u03BB", "lambda "))
        ConcatNames = fileNameFromPaths paths
        InputLines = 
            if internalInput.Length > 0 then
                {
                Input = "INTERNAL"
                Lines = internalInput.Split '\n'
                PriorLineCount = 0
                }::snd input
                |> List.rev
            else
                snd input 
                |> List.rev
        }
