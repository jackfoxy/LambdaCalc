namespace Jackfoxy.LambdaCalc

open Common
open PrettyPrint

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

module Reduce =

    type ReduceParams =
        {
            AddBinding : Context -> string -> Binding -> Context
            Eval : Context -> Term -> Term
            EvalBinding : Context -> Binding -> Binding
            PrintTerm : bool -> Context -> Term -> unit
            PrintBinding : Context -> Binding -> unit
            InputLines : InputLines list
        }

    let processCommand (reduceParams : ReduceParams) commentLines (inputLines : InputLines list) ctx cmd = 
        match cmd with
        | Eval(_, term) -> 
            let t' = reduceParams.Eval ctx term
            let reducedCommentLines, remainingInputLines = 
                printComments term commentLines inputLines
            reduceParams.PrintTerm true ctx t'
            flush()
            ctx, reducedCommentLines, remainingInputLines

        | Bind(_, x, binding) -> 
            let reducedCommentLines, remainingInputLines =
                match binding with 
                | NameBind -> 
                    commentLines, inputLines 
                | AbstractionBind t ->
                    printComments t commentLines inputLines

            let bind' = reduceParams.EvalBinding ctx binding
            pr x
            pr " "
            reduceParams.PrintBinding ctx bind'
            flush()
            (reduceParams.AddBinding ctx x bind'), reducedCommentLines, remainingInputLines 

    let rec evalDriver (eval : Context -> Term -> Term) ctx term =
        try 
            evalDriver eval ctx <| eval ctx term 
        with | Common.NoRuleAppliesException -> term
  
    let evalBinding eval ctx binding =
        match binding with
        | AbstractionBind term -> 
            AbstractionBind <| evalDriver eval ctx term
        | bind -> 
            bind

    let reduceInput (reduceParams : ReduceParams) (ctx : Context) cmds =
        let commentLines = getCommentLines reduceParams.InputLines

        let g (ctx, commentLines, lastFilePrintedLineCount) c = 
            let results = processCommand reduceParams commentLines lastFilePrintedLineCount ctx c
            results

        let result, remainingComments, _ =
            List.fold g (ctx, commentLines, reduceParams.InputLines) cmds

        printRemainingComments remainingComments

        result