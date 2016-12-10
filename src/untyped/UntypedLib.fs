namespace Jackfoxy.LambdaCalc.Untyped

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

    let printInputSource t inputLines =
        let line = 
            match t with
            | Variable ((FI (_, line, _)), _ , _) -> line
            | Abstraction ((FI (_, line, _)), _ , _) -> line
            | Application ((FI (_, line, _)), _ , _) -> line
            | _ -> System.Int32.MaxValue

        match inputLines |> List.tryFindIndex (fun x -> x.PriorLineCount = (line - 1)) with
        | Some n -> 
            if n + 1 < inputLines.Length then
                if n > 0 then
                    pr "\n"
                pr ("/*" + (inputLines.[n+1].Input)  + "*/\n")
                flush()
        | None ->
            ()

    let processCommand ctx cmd = 
        match cmd with
        | Eval(_, t) -> 
            let t' = eval ctx t
            printTerm true ctx t'
            flush()
            ctx
        | Bind(_, x, bind) -> 
            match bind with 
            | NameBind -> () 
            | AbbstractionBind t ->
                printInputSource t inputLines
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

