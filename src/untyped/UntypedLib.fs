﻿namespace Jackfoxy.LambdaCalc.Untyped

(*
Copyright (c) 2003, Benjamin C. Pierce
Copyright (c) 2012, Jack Pappas
All rights reserved.

This code is provided under the terms of the 2-clause ("Simplified") BSD license.
See LICENSE.TXT for licensing details.
*)

(* Module UntypedLib: The main program.  Deals with processing the command
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
open Support.Error

module UntypedLib = 

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

    let processCommand commentLines ctx cmd = 
        match cmd with
        | Eval(_, t) -> 
            let t' = eval ctx t
            let reducedCommentLines = 
                printComments t commentLines
            printTerm true ctx t'
            flush()
            ctx, reducedCommentLines

        | Bind(_, x, bind) -> 
            let reducedCommentLines =
                match bind with 
                | NameBind -> 
                    commentLines 
                | AbbstractionBind t ->
                    printInputSource t inputLines
                    printComments t commentLines

            let bind' = evalBinding ctx bind
            pr x
            pr " "
            printBinding ctx bind'
            flush()
            (addBinding ctx x bind'), reducedCommentLines

    let processInput input =
        let ctx = emptyContext

        let (cmds, _) = parseInput input ctx

        let commentLines = getCommentLines inputLines
        
        let g (ctx, commentLines) c = 
            let results = processCommand commentLines ctx c
            results

        let result, remainingComments =
            List.fold g (ctx, commentLines) cmds

        printRemainingComments remainingComments

        result
