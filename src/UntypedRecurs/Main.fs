namespace Jackfoxy.LambdaCalc.UntypedRecurs

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

open Ast
open Core
open Jackfoxy.LambdaCalc
open Jackfoxy.LambdaCalc.CommandLine
open FSharp.Compatibility.OCaml.Format
open UntypedRecursLib

module console1 =

    exception ExitException of int

    [<EntryPoint>]
    let main argv = 
        
        let parsedCommand = parse (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name) argv

        let res =
            match parsedCommand.Source with
            | NoSource -> 
                reportError parsedCommand
                0
            | input -> 

                set_max_boxes 1000
                set_margin 67
        
                try 
                    (fun () -> 
                        try 
                            processInput parsedCommand input emptyContext |> ignore
                            0 
                        with 
                            | ExitException x -> x) ()
                with e ->
                    printfn "%A" e
                    2

        try
            Compatability.print_flush ()
        with _ -> ()

        printfn "Hit any key to exit."
        System.Console.ReadKey() |> ignore

        exit res
