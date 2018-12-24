(**
LambdaCalc Tutorial
===================

Use build.cmd to build the console app projects. See [Build Notes](buildnotes.html) for important information about the first time build 
and builds after making changes to the Parser.fs files.

When in PowerShell, set the output encoding to UTF-8 encoding to properly display the λ character.

````
[lang=powershell]
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
````

The following console command evaluates [example λ-abstractions and unit tests](https://github.com/jackfoxy/LambdaCalc/tree/master/lambdas) in the Untyped Lambda Calculus.
Note order of lambda files is significant, as subsequent lambdas may depend on values defined in previous files. 

````
[lang=powershell]
dotnet .\bin\Untyped\netcoreapp2.1\Untyped.dll prelude.lmbd boolean.lmbd tuple.lmbd numbers.lmbd numbers.lmbd lists.lmbd tests\cond.lmbd tests\numbers.lmbd tests\lists.lmbd -c "(λx. λy. y x) (λx. x) (lambda x.x)" -l -i .\lambdas
````

The final input in the list `-c "(λx. λy. y x) (λx. x) (lambda x.x)"` is an example of lambda calculus typed into the console and does nothing more than demonstrate 
submitting a λ-abstraction from the console after the abstractions in the files have evaluated. To submit multiple abstractions from the console be sure to `;` separate them.

See [console app documentation](index.html) for all parameter options.

Evaluatining abstractions in the Untyped Recursive Lambda Calculus:

````
[lang=powershell]
dotnet .\bin\UntypedRecurs\netcoreapp2.1\UntypedRecurs.dll prelude.lmbd boolean.lmbd tuple.lmbd numbers.lmbd numbers.lmbd lists.lmbd tests\cond.lmbd tests\numbers.lmbd tests\lists.lmbd tests\recursion.lmbd -c "(λx. λy. y x) (λx. x) (lambda x.x)" -l -i .\lambdas
````

Now you can write [unit tests](unitTests.html) in the untyped Lambda Calculus. Sure beats desk-checking the abstractions you compose. 

*)
