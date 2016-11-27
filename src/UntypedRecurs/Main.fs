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
open UntypedRecursLib

module console1 =

    exception ExitException of int

    [<EntryPoint>]
    let main argv = 
        
        let parsedCommand = parse (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name) argv

        match parsedCommand.Source with
        | NoSource -> 
            reportError parsedCommand
            0
        | input -> 

            try 
                (fun () -> 
                    try 
                        processInput input emptyContext |> ignore
                        0 
                    with 
                        | ExitException x -> x) ()
            with e ->
                printfn "%A" e
                2