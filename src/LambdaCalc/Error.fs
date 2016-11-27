(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

[<AutoOpen>]
module Support.Error

open Jackfoxy.LambdaCalc.PrettyPrint

exception ExitException of int

type Info =
    | FI of File : string * Line : int * Character : int
    | UNKNOWN
    
type WithInfo<'a> = {i : Info; v : 'a}

let dummyinfo = UNKNOWN
let createInfo f l c = FI (f, l, c)

let errf f =
    f()
    raise (ExitException 1)

let printInfo = function
    (* In the text of the book, file positions in error messages are replaced with the string "Error:" *)
    | FI (f, l, c) ->
        pr f
        pr ":"
        printInt l; pr "."
        printInt c; pr ":"
    | UNKNOWN ->
        pr "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; printSpace(); f())

let err s = errf (fun()-> pr "Error: "; pr s; forceNewline())

let error fi s = errfAt fi (fun()-> pr s; forceNewline())

let warning s =
    pr "Warning: "
    pr s
    forceNewline()

let warningAt fi s =
    printInfo fi
    pr " Warning: "
    pr s
    forceNewline()
