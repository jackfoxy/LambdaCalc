namespace Jackfoxy.LambdaCalc.UntypedRecurs

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
open CommandLine
open Common
open CommonAst
open Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open Reduce
open Support.Error

module Reduce = 
    
    [<Literal>]
    let IdentityFix = "id = lambda b. b\n;fix = lambda f. (lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y));"

    let mutable inputLines : InputLines list = []

    let parseInput (inputSource : Source) secondaryInput = 

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
            LexBuffer<char>.FromString (IdentityFix + (s'.Replace("\u03BB", "lambda ")) )
            |> parseIt 
        | Source.File paths -> 
            let input = getInput (Some IdentityFix) paths secondaryInput

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
            Eval = eval
            EvalBinding = evalBinding
            PrintTerm = printTerm
            PrintBinding = printBinding
            InputLines = inputLines
            }

        reduceInput reduceParams ctx cmds
