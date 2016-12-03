namespace Jackfoxy.LambdaCalc.UntypedRecurs

open Ast
open Core
open Jackfoxy.LambdaCalc
open Jackfoxy.LambdaCalc.CommandLine
open UntypedRecursLib

module Console =

    [<EntryPoint>]
    let main argv = 
        
        let parsedCommand = parse (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name) argv

        match parsedCommand.Source with
        | NoSource -> 
            reportError parsedCommand
            1
        | input -> 

            PrettyPrint.useLambda <- parsedCommand.Lambda

            try 
                processInput input emptyContext |> ignore
                PrettyPrint.flush()
                0 
            with e ->
                printfn "%A" e
                2