namespace Jackfoxy.LambdaCalc.Untyped

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module Reduce: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Jackfoxy.LambdaCalc
open Common
open CommonAst
open CommandLine
open Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open PrettyPrint
open Reduce
open Support.Error

/// Lambda reduction to normal form through call by value.
module Reduce = 

    let mutable private inputLines : InputLines list = []

    let private parseInput (inputSource : Source) secondaryInput = 

        let parseIt lexbuf = 
            Lexer.lineno := 1

            try 
                Parser.toplevel Lexer.main lexbuf
            with Parsing.RecoverableParseError -> 
                error (Lexer.info lexbuf) "Parse error (missing ending ; ?)"

        match inputSource with
        | Source.Console s ->
            let s' = 
                if s.EndsWith(";") then s
                else s + ";"

            let consoleInput = s'.Replace("\u03BB", "lambda ")

            LexBuffer<char>.FromString (consoleInput) 
            |> parseIt

        | Source.File paths -> 
            let input = getInput None paths secondaryInput

            Lexer.filename := input.ConcatNames
                
            let out =
                LexBuffer<char>.FromTextReader input.InputReader 
                |> parseIt

            input.InputReader.Dispose()
            inputLines <- input.InputLines
            out

        | _ -> invalidArg "can't get here" ""

    let processInput (input : Source list) =
        let ctx = emptyContext

        let (cmds, _) =
            match input with
            | [hd] ->
                parseInput hd None ctx 
            | files::[(Source.Console s)] ->
                parseInput files (Some s) ctx 
            | _ -> 
                    invalidArg "processInput" "can't get here"

        let reduceParams = 
            {
                AddBinding = addBinding
                Eval = (evalDriver eval)
                EvalBinding = (evalBinding eval)
                PrintTerm = printTerm
                PrintBinding = printBinding
                InputLines = inputLines
            }

        reduceInput reduceParams ctx cmds
