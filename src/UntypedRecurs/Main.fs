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
            0
        | input -> 

            PrettyPrint.useLambda <- parsedCommand.Lambda

            try 
                processInput input emptyContext |> ignore
                0 
            with e ->
                printfn "%A" e
                2