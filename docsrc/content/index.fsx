(**
LambdaCalc
======================

Lambda Calculus parsers using [FsLexYacc](http://fsprojects.github.io/FsLexYacc/). The lexers and parsers are as minimalistic as possible, 
supporting Untyped Lambda Calculus syntax and comments. The resulting console and .NETCore applications function as working interpreters 
performing evaluation and reduction.

Untyped Lambda Calculus
----------------------------

The console app Untyped is a parser/evalutor for the Untyped Lambda Calculus.

Untyped Recursive Lambda Calculus
---------------------------------

The console app UntypedRecurs is a parser/evalutor for the Untyped Recursive Lambda Calculus, a demonstration project in conjunction with 
a recently submitted research paper, "Recursion and the Bottom of Lambda Calculus". It successfully recurses functions that would otherwise 
not terminate under evaluation by value, so long as the function emits the bottom abstraction, `λt.λb. b`, when it is finished recursing.

Documentation
-------------

````
USAGE: Untyped [--help] [--consoleinput <string>] [--inputfolder <string>] [--lambda] [<string>...]

INPUTPATHS:

    <string>...           (optional) list file paths of input in order to process

OPTIONS:

    --consoleinput, -c <string>
                          input from console for process after optional files
    --inputfolder, -i <string>
                          folder for input paths
    --lambda, -l          use and print u03BB for lambda, followed by space
    --help                display this list of options.
````

λ-abstraction terms must be semi-colon delimited `;`. 

Comments begin with `/*`, end with `*/`, and may span lines.

When running in PowerShell, set the output encoding to UTF-8 encoding to properly display the `λ` character.

````
[lang=powershell]
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
````

 * [Build Notes](buildnotes.html) describes the simple 2-step manual process required to make the first build.

 * [Tutorial](tutorial.html) contains a further explanation of this project.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the project. 
 
Acknowledgements
----------------

This work is based on original source code By Benjamin C. Pierce in support of his classic text, 
[Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/).

Jack Pappas created the original [F# port](https://github.com/jack-pappas/fsharp-tapl) of Pierce's code. 
[Jackfoxy's fork](https://github.com/jackfoxy/fsharp-tapl) of Pappas' work builds with recent [F#](http://fsharp.org/) 
and [FsLexYacc](http://fsprojects.github.io/FsLexYacc/) releases.

Most of Pierce's files have been heavily modified for this project. Pretty printing was completely rewritten.

Contributing and copyright
--------------------------

This project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests.

The library is available under permissive licensing, which allows modification and 
redistribution for both commercial and non-commercial purposes, provided files 
copyrighted by Benjamin C. Pierce retain their copyright notice. For more information see the [License file][license] in the GitHub repository. 

  [content]: https://github.com/jackfoxy/LambdaCalc/tree/master/docs/content
  [gh]: https://github.com/jackfoxy/LambdaCalc
  [issues]: https://github.com/jackfoxy/LambdaCalc/issues
  [readme]: https://github.com/jackfoxy/LambdaCalc/blob/master/README.md
  [license]: https://github.com/jackfoxy/LambdaCalc/blob/master/LICENSE.txt
*)
