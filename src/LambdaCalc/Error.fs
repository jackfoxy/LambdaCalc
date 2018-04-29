(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

[<AutoOpen>]
/// Parsing support module.
module Support.Error

open Jackfoxy.LambdaCalc.ErrorPrint

exception internal ExitException of int

/// Input file information.
type Info =
    /// File path, line number, and character position.
    | FI of File : string * Line : int * Character : int
    /// Unknown file.
    | UNKNOWN
    
type WithInfo<'a> = {i : Info; v : 'a}

/// Dummy file info.
let dummyinfo = UNKNOWN

/// Create file info.
let createInfo f l c = FI (f, l, c)

let internal errf f =
    f()
    raise (ExitException 1)

let internal printInfo = function
    (* In the text of the book, file positions in error messages are replaced with the string "Error:" *)
    | FI (f, l, c) ->
        pr f
        pr ":"
        printInt l; pr "."
        printInt c; pr ":"
    | UNKNOWN ->
        pr "<Unknown file and line>: "

let internal errfAt fi f = errf(fun()-> printInfo fi; printSpace(); f())

let internal err s = errf (fun()-> pr "Error: "; pr s; flush())

/// Parsing error.
let error fi s = errfAt fi (fun()-> pr s; flush())
