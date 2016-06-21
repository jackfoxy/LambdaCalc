(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

namespace FSharpTapl

open Ast
open CommandLine
open Core
open FulluntypedLib

module console1 =
    [<EntryPoint>]
    let main argv = 

        let parsedCommand = parse argv

        match parsedCommand.Source with
        | NoSource -> 
            reportError parsedCommand
        | input -> 
            let main () =
                processInput parsedCommand input emptyContext |> ignore

            Common.runMain main
            ()

        0


                
