namespace Jackfoxy.LambdaCalc

module PrettyPrint =

    type PrintState =
        {
        Offset : int
        LineLength : int
        ParenIndex : int
        ParenOffset : int list
        PriorPrint : string
        }

    let private indentTab = 1

    let private maxLambdaLine = 80

    let mutable useLambda = false

    let mutable private buffer : string list = []

    let notParen x =
        x <> "(" && x <> ")"

    let toBuffer s =
        match buffer with
        | [] -> 
            buffer <- s::buffer
        | "("::t 
        | ")"::t ->
            buffer <- s::buffer
        | x::t ->
            if notParen s then
                buffer <- (x + s)::t
            else
                buffer <- s::x::t

    let printInt (i : int) = 
        i.ToString()
        |> toBuffer

    let pr (s : string) = 
        let s' = if useLambda then s.Replace("lambda ", "\u03BB") else s

        toBuffer s'

    let printSpace() =
        toBuffer " "

    let sumOfLengths (s : string []) =
        s
        |> Array.fold (fun s t -> s + t.Length) 0
        
    let rec prepLoop (front : string list) buf =
        match buf with
        | "("::x::")"::[] when front.IsEmpty  ->
            [x]

        | "("::x::")"::t when notParen x && x.Length < (maxLambdaLine / 2) ->
            let revFront = List.rev front
            prepLoop [] (revFront @ (("(" + x + ")")::t))

        | "("::x::y::")"::t when notParen x && notParen y && sumOfLengths [|x;y|] < (maxLambdaLine / 2) ->
            let revFront = List.rev front
            prepLoop [] (revFront @ (("(" + x + y + ")")::t))

        | "("::x::y::z::")"::t when notParen x && notParen y && sumOfLengths [|x;y;z|] < (maxLambdaLine / 2) ->
            let revFront = List.rev front
            prepLoop [] (revFront @ (("(" + x + y + z + ")")::t))

        | "("::w::x::y::z::")"::t when notParen x && notParen y && sumOfLengths [|w;x;y;z|] < (maxLambdaLine / 2) ->
            let revFront = List.rev front
            prepLoop [] (revFront @ (("(" + w + x + y + z + ")")::t))

        | x::y::t when notParen x && notParen y ->
            let revFront = List.rev front
            prepLoop [] (revFront @ ((x + y)::t))

        | h::t ->
            prepLoop (h::front) t

        | [] ->
            List.rev front

    let printToNewLine (printState : PrintState) (s : string) =
        let s' = s.TrimStart(' ')

        if s'.Length = 0 then
            ()
        else
            let parenOffset =
                if printState.ParenOffset.Length = 0 then
                    printState.Offset + indentTab
                else
                    printState.ParenOffset.Head
            Microsoft.FSharp.Core.Printf.printf "\n%s%s" 
                (new System.String(' ', (parenOffset + indentTab)))
                s'
 
    let getNewState printState s =
        match s with
        | "(" ->
            { printState with 
                LineLength = printState.LineLength + s.Length
                ParenIndex = printState.ParenIndex + 1
                ParenOffset = (printState.LineLength + 1)::printState.ParenOffset 
                PriorPrint = "("}
        | ")" ->
            { printState with 
                LineLength = printState.LineLength + s.Length
                ParenIndex = printState.ParenIndex - 1
                ParenOffset = printState.ParenOffset.Tail 
                PriorPrint = ")"}
        | _ ->
            { printState with 
                LineLength = printState.LineLength + s.Length 
                PriorPrint = s}

    let rec printLoop printState buf =
        
        match buf with
        | "("::x::")"::t when notParen x && (printState.PriorPrint.EndsWith(".") |> not )  ->
            printToNewLine printState ("(" + x + ")")
            let newState = 
                { printState with 
                    LineLength = maxLambdaLine
                    PriorPrint = ")" }
            printLoop newState t

        | "("::"("::"("::x::t when notParen x && (sumOfLengths [|"(((";x|] + printState.LineLength) > maxLambdaLine ->
            printToNewLine printState ("(((" + x)
            let newState = 
                { printState with 
                    LineLength = sumOfLengths [|"(((";x|]
                    ParenIndex = printState.ParenIndex + 3
                    ParenOffset = (printState.LineLength + 1)::(printState.LineLength + 2)::(printState.LineLength + 3)::printState.ParenOffset 
                    PriorPrint = x }
            printLoop newState t

        | "("::"("::x::t when notParen x && (sumOfLengths [|"((";x|] + printState.LineLength) > maxLambdaLine ->
            printToNewLine printState ("((" + x)
            let newState = 
                { printState with 
                    LineLength = sumOfLengths [|"((";x|]
                    ParenIndex = printState.ParenIndex + 2
                    ParenOffset = (printState.LineLength + 1)::(printState.LineLength + 2)::printState.ParenOffset
                    PriorPrint = x }
            printLoop newState t

        | "("::x::t when notParen x && (sumOfLengths [|"(";x|] + printState.LineLength) > maxLambdaLine ->

            match buf with
            | "("::x::")"::t when notParen x && (printState.PriorPrint.EndsWith(".")) && (sumOfLengths [|"(";x;")"|] + printState.LineLength) > maxLambdaLine  ->
                printToNewLine printState ("(" + x + ")")
                let newState = 
                    { printState with 
                        LineLength = maxLambdaLine
                        PriorPrint = ")" }
                printLoop newState t

            | _ ->
                printToNewLine printState ("(" + x)
                let newState = 
                    { printState with 
                        LineLength = sumOfLengths [|"(";x|]
                        ParenIndex = printState.ParenIndex + 1
                        ParenOffset = (printState.LineLength + 1)::printState.ParenOffset
                        PriorPrint = x }
                printLoop newState t

        | x::")"::")"::")"::t when notParen x && (sumOfLengths [|")))";x|] + printState.LineLength) > maxLambdaLine ->
            printToNewLine printState (x + ")))")
            let newState = 
                { printState with 
                    LineLength = maxLambdaLine 
                    ParenIndex = printState.ParenIndex - 3
                    ParenOffset = printState.ParenOffset.Tail.Tail.Tail
                    PriorPrint = ")" }
            printLoop newState t

        | x::")"::")"::t when notParen x && (sumOfLengths [|"))";x|] + printState.LineLength) > maxLambdaLine ->
            printToNewLine printState (x + "))")
            let newState = 
                { printState with 
                    LineLength = maxLambdaLine 
                    ParenIndex = printState.ParenIndex - 2
                    ParenOffset = printState.ParenOffset.Tail.Tail
                    PriorPrint = ")" }
            printLoop newState t

        | x::")"::t when notParen x && (sumOfLengths [|")";x|] + printState.LineLength) > maxLambdaLine ->
            printToNewLine printState (x + ")")
            let newState = 
                { printState with 
                    LineLength = maxLambdaLine 
                    ParenIndex = printState.ParenIndex - 1
                    ParenOffset = printState.ParenOffset.Tail
                    PriorPrint = ")" }
            printLoop newState t

        | h::t when printState.LineLength + h.Length > maxLambdaLine ->
            printToNewLine printState h
            let newState = getNewState printState h
            printLoop newState  t

        | h::t ->
            Microsoft.FSharp.Core.Printf.printf "%s" h
            let newState = getNewState printState h
            printLoop newState t

        | [] -> ()

    let flush() =
        let offset = 
            if buffer.Length = 0 then
                0
            else
                (buffer.[buffer.Length - 1].IndexOf("=")) + 2

        let printState =
            {
            Offset = offset
            LineLength = 0
            ParenIndex = 0
            ParenOffset = []
            PriorPrint = ""
            }

        let preppedBuffer =
            buffer
            |> List.rev
            |> prepLoop []
        preppedBuffer
        |> printLoop printState

        Microsoft.FSharp.Core.Printf.printf "%s" "\n"

        buffer <- []
