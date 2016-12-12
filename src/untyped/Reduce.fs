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

open Ast
open Jackfoxy.LambdaCalc
open Common
open CommonAst
open CommandLine
open PrettyPrint
open Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open Reduce
open Support.Error

module Reduce = 

    let mutable inputLines : InputLines list = []

    let parseInput (inputSource : Source) = 

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
            let input = getInput "" paths

            Lexer.filename := input.ConcatNames
                
            let out =
                LexBuffer<char>.FromTextReader input.InputReader 
                |> parseIt

            input.InputReader.Dispose()
            inputLines <- input.InputLines
            out

        | _ -> invalidArg "can't get here" ""

    let processInput input =
        let ctx = emptyContext

        let (cmds, _) = parseInput input ctx

        let reduceParams = 
            {
            AddBinding = addBinding
            Eval = eval
            EvalBinding = evalBinding
            PrintTerm = printTerm
            PrintBinding = printBinding
            InputLines = inputLines
            }

        let commentLines = getCommentLines inputLines

        reduceInput reduceParams commentLines ctx cmds
