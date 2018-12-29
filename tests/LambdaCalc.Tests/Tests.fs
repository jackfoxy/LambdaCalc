namespace LambdaCalc.Tests

open Expecto
open System
open System.Diagnostics

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

    let lambdaTruth = "λt.λf.t"

    type LambdaType =
        | Untyped 
        | UntypedRecurs

    let runLambdaTest (typeDll : LambdaType) requiredFiles assertion =
        let typeDirectory =
            match typeDll with
            | Untyped ->
                Some untypedDirectory
            | UntypedRecurs ->
                Some untypedRecursDirectory

        let files =
            requiredFiles
            |> Array.reduce (fun acc item -> acc + " " + item)

        let args = sprintf "%s.dll %s -c \"%s\" -l -i ..\..\..\lambdas" (typeDll.ToString()) files assertion

        let msg = sprintf "%s %s expected true" (typeDll.ToString()) assertion

        let output, _ = runProcess "dotnet" args typeDirectory
        let result = output |> Seq.last

        Expect.isTrue (result = lambdaTruth) msg

    [<Tests>]
    let testCond =

        testList "Cond" [
            testCase "untyped cond tru tru fls" <| fun () ->
                runLambdaTest LambdaType.Untyped [|"prelude.lmbd"; "boolean.lmbd"|] "xnor tru (cond tru tru fls)"

            testCase "untypedrecurs cond tru tru fls" <| fun () ->
                runLambdaTest LambdaType.UntypedRecurs [|"prelude.lmbd"; "boolean.lmbd"|] "xnor tru (cond tru tru fls)"

        ]
