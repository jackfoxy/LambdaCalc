namespace LambdaCalc.Tests

open Expecto
open System
open System.Diagnostics
open System.Text

module Tests =
    let runProcess filename args startDir = 
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )
        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data

        use p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))

        let started = 
            try
                p.Start()
            with | ex ->
                ex.Data.Add("filename", filename)
                reraise()
        if not started then
            failwithf "Failed to start process %s" filename
    
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        let cleanOut l = l |> Seq.filter (String.IsNullOrEmpty >> not)
        cleanOut outputs,cleanOut errors


    let getGrandParent path =
        let dir =
            System.IO.Directory.GetParent(path).FullName
            |> System.IO.Directory.GetParent
        dir.FullName

    let untypedDirectory =
        sprintf "%s\\bin\\Untyped\\netcoreapp2.1" <| getGrandParent __SOURCE_DIRECTORY__

    let untypedRecursDirectory =
        sprintf "%s\\bin\\UntypedRecurs\\netcoreapp2.1" <| getGrandParent __SOURCE_DIRECTORY__

    let lambdaTrue = "λt.λf.t"
    let lambdaFalse = "λt.λf.f"

    type LambdaType =
        | Untyped 
        | UntypedRecurs
        | Both

    let lambdaTest (typeDll : LambdaType) requiredFiles assertion =
        let typeDirectory =
            match typeDll with
            | Untyped ->
                Some untypedDirectory
            | UntypedRecurs ->
                Some untypedRecursDirectory
            | Both ->
                None //can't get here

        let files =
            requiredFiles
            |> Array.reduce (fun acc item -> acc + " " + item)

        let args = sprintf "%s.dll %s -c \"%s\" -l -i ..\..\..\lambdas" (typeDll.ToString()) files assertion

        let msg = sprintf "%s %s expected " (typeDll.ToString()) assertion

        let output, _ = runProcess "dotnet" args typeDirectory
        let result = output |> Seq.last

        printfn "result %A" <| Encoding.Unicode.GetBytes(result)
        printfn "expected %A" <| Encoding.Unicode.GetBytes(lambdaTrue)

        Expect.isTrue (result = lambdaTrue) (sprintf "%s %s = %s" msg result lambdaTrue)

    let runLambdaTest (typeDll : LambdaType) requiredFiles assertion =
        match typeDll with
        | Untyped 
        | UntypedRecurs ->
            lambdaTest typeDll requiredFiles assertion
        | Both ->
            lambdaTest LambdaType.Untyped requiredFiles assertion
            lambdaTest LambdaType.UntypedRecurs requiredFiles assertion

    [<Tests>]
    let testCond =

        testList "Cond" [
            testCase "cond tru tru fls" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"|] "xnor tru (cond tru tru fls)"

            testCase "cond fls fls tru" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"|] "xnor tru (cond fls fls tru)"

            testCase "cond fls tru fls (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"|] "xnor fls (cond fls tru fls)"

        ]

    [<Tests>]
    let testNumbers =

        testList "Numbers" [
            testCase "exponate eql c1 (exp c1 c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (exp c1 c0))"

            testCase "exponate eql c1 (exp c1 c1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (exp c1 c1))"

            testCase "exponate eql c1 (exp c1 c2)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (exp c1 c2))"

            testCase "exponate eql c1 (exp c2 c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (exp c2 c0))"

            testCase "exponate eql c2 (exp c2 c1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c2 (exp c2 c1))"

            testCase "exponate eql c4 (exp c2 c2)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c4 (exp c2 c2))"

            testCase "exponate eql c8 (exp c2 c3)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c8 (exp c2 c3))"

            testCase "exponate eql c9 (exp c3 c2)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c9 (exp c3 c2))"


            testCase "plus eql c1 (plus c1 c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (plus c1 c0))"

            testCase "plus eql c1 (plus c0 c1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (plus c0 c1))"

            testCase "plus eql c2 (plus c2 c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c2 (plus c2 c0))"

            testCase "plus eql c1 (plus c2 c0) (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor fls (eql c1 (plus c2 c0))"

            testCase "plus eql c6 (plus c2 c4)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c6 (plus c2 c4))"

            testCase "plus eql c6 (plus c4 c2)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c6 (plus c4 c2))"


            testCase "successor eql c1 (scc c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (scc c0))"

            testCase "successor eql c2 (scc c1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c2 (scc c1))"

            testCase "successor eql c6 (scc c5)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c6 (scc c5))"

            testCase "successor eql c5 (scc c5) (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor fls (eql c5 (scc c5))"


            testCase "predecessor eql c0 (prd c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c0 (prd c0))"

            testCase "predecessor eql c0 (prd c1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c0 (prd c1))"

            testCase "predecessor eql c1 (prd c2)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c1 (prd c2))"

            testCase "predecessor eql c5 (prd c6)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c5 (prd c6))"

            testCase "predecessor eql c6 (prd c6) (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor fls (eql c6 (prd c6))"


            testCase "is zero iszro (prd c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (iszro (prd c0))"

            testCase "is zero iszro (prd c1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (iszro (prd c1))"

            testCase "is zero iszro (scc c0) (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor fls (iszro (scc c0))"

            testCase "is zero iszro (prd c6) (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor fls (iszro (prd c6))"


            testCase "multiply iszro (mult c1 c0)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (iszro (mult c1 c0))"

            testCase "multiply eql c6 (mult c2 c3)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c6 (mult c2 c3))"

            testCase "multiply eql c5 (mult c2 c3) (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor fls (eql c5 (mult c2 c3))"

            testCase "multiply eql c20 (mult c4 c5)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c20 (mult c4 c5))"

            testCase "multiply eql c24 (mult c4 c6)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c24 (mult c4 c6))"

            testCase "multiply eql c25 (mult c5 c5)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c25 (mult c5 c5))"


            testCase "25 = 5 + 4 * 5" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "xnor tru (eql c25 (plus c5 (mult c4 c5)))"
        ]

    [<Tests>]
    let testLists =

        testList "Lists" [
            testCase "isnil nil" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "xnor tru (isnil nil)"

            testCase "one member list eql c0 (head lst1)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "lst1 = cons c0 nil; xnor tru (eql c0 (head lst1))"

            testCase "one member list isnil lst1 (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "lst1 = cons c0 nil; xnor fls (isnil lst1)"

            testCase "two member list head of lst2" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "lst1 = cons c0 nil; lst2 = cons c1 lst1; xnor tru (eql c1 (head lst2))"

            testCase "two member list head of tail of lst2" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "lst1 = cons c0 nil; lst2 = cons c1 lst1; xnor tru (eql c0 (head (tail lst2)))"

            testCase "two member list isnil lst2 (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "lst1 = cons c0 nil; lst2 = cons c1 lst1; xnor fls (isnil lst2)"

            testCase "two member list isnil tail of tail" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "lst1 = cons c0 nil; lst2 = cons c1 lst1; xnor tru (isnil (tail (tail lst2)))"

            testCase "one member false list head" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1f = cons fls nil; xnor tru (xnor fls (head lst1f))"

            testCase "one member false list isnil (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1f = cons fls nil; xnor fls (isnil lst1f)"

            testCase "two member false list head" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1f = cons fls nil; lst2f = cons fls lst1f; xnor tru (xnor fls (head lst2f))"

            testCase "two member false list head of tail" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1f = cons fls nil; lst2f = cons fls lst1f; xnor tru (xnor fls (head (tail lst2f)))"

            testCase "two member false list isnil (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1f = cons fls nil; lst2f = cons fls lst1f; xnor fls (isnil lst2f)"

            testCase "two member false list isnil tail of tail" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1f = cons fls nil; lst2f = cons fls lst1f; xnor tru (isnil (tail (tail lst2f)))"

            testCase "one member true list head" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1t = cons tru nil; xnor tru (xnor tru (head lst1t))"

            testCase "one member true list isnil (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1t = cons tru nil; xnor fls (isnil lst1t)"

            testCase "two member true list head" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1t = cons tru nil; lst2t = cons tru lst1t; xnor tru (xnor tru (head lst2t))"

            testCase "two member true list head of tail" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1t = cons tru nil; lst2t = cons tru lst1t; xnor tru (xnor tru (head (tail lst2t)))"

            testCase "two member true list isnil (false)" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1t = cons tru nil; lst2t = cons tru lst1t; xnor fls (isnil lst2t)"

            testCase "two member true list isnil tail of tail" <| fun () ->
                runLambdaTest LambdaType.Both [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "lists.lmbd"|] "lst1t = cons tru nil; lst2t = cons tru lst1t; xnor tru (isnil (tail (tail lst2t)))"
        ]

    [<Tests>]
    let testRecursion =

        testList "Recursion" [
            testCase "fact2 = c2" <| fun () ->
                runLambdaTest LambdaType.UntypedRecurs [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "fact = λf. λx. mult x (f (prd x)); fact2 = fix fact c2; xnor tru (eql c2 fact2)"

            testCase "fact3 = c3 (false)" <| fun () ->
                runLambdaTest LambdaType.UntypedRecurs [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "fact = λf. λx. mult x (f (prd x)); fact3 = fix fact c3; xnor fls (eql c3 fact3)"

            testCase "fact3 = c6" <| fun () ->
                runLambdaTest LambdaType.UntypedRecurs [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "fact = λf. λx. mult x (f (prd x)); fact3 = fix fact c3; xnor tru (eql c6 fact3)"

            testCase "fact4 = c24" <| fun () ->
                runLambdaTest LambdaType.UntypedRecurs [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"|] "fact = λf. λx. mult x (f (prd x)); fact4 = fix fact c4; xnor tru (eql c24 fact4)"

            testCase "head of taillist = c1" <| fun () ->
                runLambdaTest LambdaType.UntypedRecurs [|"prelude.lmbd"; "boolean.lmbd"; "tuple.lmbd"; "numbers.lmbd"; "lists.lmbd"|] "list3 = cons c3 (cons c2 (cons c1 nil)); rev = λf. λx. cond (isnil (tail x)) (head x) (f (cond (isnil (tail x)) nil (cons (cons (head (tail x)) (head x)) (tail (tail x)) )) ); taillist = fix rev (cons nil list3); xnor tru (eql c1 (head taillist))"
        ]   