namespace Jackfoxy.LambdaCalc.UntypedRecurs

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module UntypedRecursLib: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Ast
open Jackfoxy.LambdaCalc
open CommandLine
open PrettyPrint
open Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open Support.Error
open System.IO

module UntypedRecursLib = 
    
    [<Literal>]
    let BottomFix =
        @"bottom = lambda t.lambda b. b;
fix = lambda f. (lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y));"

    let inputReader (paths : string list) =
        let streams = 
            paths
            |> List.map (fun x -> new StreamReader(x))
        let input =
            streams
            |> List.fold (fun s t -> 
                let x = t.ReadToEnd()
                t.Dispose()
                s + x) ""
        new System.IO.StringReader(BottomFix + (input.Replace("\u03BB", "lambda ")))

    let parseInput (input : Source) = 

        let parseIt lexbuf = 
            Lexer.lineno := 1

            try 
                Parser.toplevel Lexer.main lexbuf
            with Parsing.RecoverableParseError -> 
                error (Lexer.info lexbuf) "Parse error"

        match input with
        | Source.Console s -> 
            LexBuffer<char>.FromString (BottomFix + (s.Replace("\u03BB", "lambda ")) )
            |> parseIt 
        | Source.File paths -> 
            use textReader = inputReader paths
            Lexer.filename := fileNameFromPaths paths

            LexBuffer<char>.FromTextReader textReader
            |> parseIt
        | _ -> invalidArg "can't get here" ""
    
    let rec processCommand ctx cmd = 
        match cmd with
        | Eval(_, t) -> 
            let t' = eval ctx t
            printTerm true ctx t'
            forceNewline()
            ctx
        | Bind(_, x, bind) -> 
            let bind' = evalBinding ctx bind
            pr x
            pr " "
            printBinding ctx bind'
            forceNewline()
            addBinding ctx x bind'
    
    let processInput input ctx = 
        let (cmds, _) = parseInput input ctx
        
        let g ctx c = 
            let results = processCommand ctx c
            results
        List.fold g ctx cmds
