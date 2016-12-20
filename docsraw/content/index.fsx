(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/untyped"

(**
LambdaCalc
======================

Stripped-down Lambda Calculus parser using fslexyacc. Lexer and parser intended to be as minimalistic as possible, 
supporting Untyped Lambda Calculus syntax and comments. Also functions as a working interpreter by performing evaluation and reduction.

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "LambdaCalc.dll"
open LambdaCalc

printfn "hello = %s" <| "world"

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Build Notes](buildnotes.html) describe a simple 2-step manual process required to make the first build..

 * [Tutorial](tutorial.html) contains a further explanation of this project.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the project. 
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under permissive licensing, which allows modification and 
redistribution for both commercial and non-commercial purposes, provided files 
copyrighted by Benjamin C. Pierce retain their copyright notice. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/jackfoxy/LambdaCalc/tree/master/docs/content
  [gh]: https://github.com/jackfoxy/LambdaCalc
  [issues]: https://github.com/jackfoxy/LambdaCalc/issues
  [readme]: https://github.com/jackfoxy/LambdaCalc/blob/master/README.md
  [license]: https://github.com/jackfoxy/LambdaCalc/blob/master/LICENSE.txt
*)
