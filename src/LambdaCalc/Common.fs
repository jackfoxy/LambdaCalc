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

    let secondFixed (secondary : string) =
        if secondary.EndsWith(";") then
            secondary
        else
            secondary + ";"

    let getInputLines secondaryInput (input : string * InputLines list) =
        match secondaryInput with
        | Some x ->
            let inputLines = snd input

            match inputLines with
            | [] ->
                inputLines
                |> List.rev
            | hd::tl ->
                {
                Input = "CONSOLE"
                Lines = [||]
                PriorLineCount = hd.PriorLineCount + hd.Lines.Length }::inputLines
                |> List.rev
                    
        | None ->
            snd input
            |> List.rev

    let getInput (internalInput : string option) (paths : string list) secondaryInput =

        let startList =
            match internalInput with
            | Some intern ->
                [{
                    Input = "INTERNAL"
                    Lines = intern.Split '\n'
                    PriorLineCount = 0 }]
                        
            | None->
                []

        let input =
            paths
            |> List.map (fun x -> (new StreamReader(x)), x )
            |> List.fold createInput ("", startList)

        let inputString =
            (match internalInput, secondaryInput with
                | (Some intern), (Some secondary) ->
                    intern + "\n" + (fst input) + "\n" + (secondFixed secondary)
                | _, (Some secondary) ->
                    (fst input) + "\n" + (secondFixed secondary)
                | (Some intern), _ ->
                    intern + "\n" + (fst input)
                | _, _ ->
                    fst input ).Replace("\u03BB", "lambda ")

        {
        InputReader = new System.IO.StringReader(inputString)
        ConcatNames = fileNameFromPaths paths
        InputLines = getInputLines secondaryInput input
        }

    type CommentLine =
        {
        LineNbr : int
        Comment : string
        }

    let getCommentLines (inputLines : InputLines list) =
        let (_, _, commentLines) =
            inputLines
            |> List.collect (fun x -> 
                x.Lines
                |> List.ofArray)
            |> List.fold (fun (lineNbr, commentIsOpen, commentLines) t -> 
                let t' = t.Trim()
                match t'.StartsWith("/*"), commentIsOpen, (t'.EndsWith("*/") && t'.Contains("/*") |> not) with
                | true, _, true ->
                    (lineNbr + 1), false, 
                        {
                        LineNbr = lineNbr
                        Comment = t'
                        }::commentLines

                | true, _, false ->
                    (lineNbr + 1), true, 
                        {
                        LineNbr = lineNbr
                        Comment = t'
                        }::commentLines

                | _, true, true ->
                    (lineNbr + 1), false, 
                        {
                        LineNbr = lineNbr
                        Comment = t'
                        }::commentLines

                | _, true, _ ->
                    (lineNbr + 1), true, 
                        {
                        LineNbr = lineNbr
                        Comment = t'
                        }::commentLines

                | _, _, _ ->
                    (lineNbr + 1), false, commentLines 
                    ) (1, false, [])

        commentLines
        |> List.rev