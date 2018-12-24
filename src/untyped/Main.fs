namespace Jackfoxy.LambdaCalc.Untyped

open Jackfoxy.LambdaCalc
open Common
open CommandLine
open Reduce

/// The console app.
module Console =

    [<EntryPoint>]
    let main argv = 

        let parsedCommand = parse (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name) argv

        match parsedCommand.Source with
        | [] -> 
            reportError parsedCommand
            1
        | input -> 

            PrettyPrint.useLambda <- parsedCommand.Lambda
            
            try 
                processInput input |> ignore
                0 
            with e ->
                printfn "%A" e
                2
