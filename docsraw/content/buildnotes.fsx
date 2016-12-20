(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/untyped"
#r "LambdaCalc.dll"

(**
Build Notes
===========

The initial build requires 2 steps:

* Step 1 -- run build.cmd

This will result in error messages

    error FS0039: The type 'Toplevel' is not defined  /LambdaCalc/src/Untyped/Untyped.fsproj

    error FS0039: The type 'Toplevel' is not defined  /LambdaCalc/src/UntypedRecurs/UntypedRecurs.fsproj


* Step 2 -- add the following `open` statements and `type` declaration at the top of the Parser module in the files `Parser.fsi`
generated by the first step in the projects Untyped and UntypedRecurs
*)
module Parser

open Support.Error
open Jackfoxy.LambdaCalc
open Jackfoxy.LambdaCalc.CommonAst

type Toplevel = Context -> (Command list * Context)
(**
Run build.cmd again.

When you regenerate Parser.fsi (for example by changing the parse file, `Parser.fsy`) you will have to repeat this procedure. 

Or if your are ambitious, submit a pull request to [FsLexYacc](https://github.com/fsprojects/FsLexYacc) to fix this in `fsi` generation.
*)
