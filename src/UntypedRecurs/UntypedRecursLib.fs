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
open Common
open PrettyPrint
open Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open Support.Error
open System.IO

module UntypedRecursLib = 
    
    [<Literal>]
    let BottomFix = "bottom = lambda t.lambda b. b;\nfix = lambda f. (lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y));\n"

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
            LexBuffer<char>.FromString (BottomFix + (s'.Replace("\u03BB", "lambda ")) )
            |> parseIt 
        | Source.File paths -> 
            let input = getInput BottomFix paths

            Lexer.filename := input.ConcatNames
                
            let out =
                LexBuffer<char>.FromTextReader input.InputReader 
                |> parseIt

            input.InputReader.Dispose()
            out

        | _ -> invalidArg "can't get here" ""
    
    let rec processCommand ctx cmd = 
        match cmd with
        | Eval(_, t) -> 
            let t' = eval ctx t
            printTerm true ctx t'
            flush()
            ctx
        | Bind(_, x, bind) -> 
            let bind' = evalBinding ctx bind
            pr x
            pr " "
            printBinding ctx bind'
            flush()
            addBinding ctx x bind'
    
    let processInput input = 
        let ctx = emptyContext

        let (cmds, _) = parseInput input ctx
        
        let g ctx c = 
            let results = processCommand ctx c
            results
        List.fold g ctx cmds
