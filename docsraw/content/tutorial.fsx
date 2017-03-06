(**
LambdaCalc Tutorial
===================

Use build.cmd to build the console app projects. See [Build Notes](buildnotes.html) for important information about the first time build 
and builds after making changes to the Parser.fs files.

Evaluating [example λ-abstractions and unit tests](https://github.com/jackfoxy/LambdaCalc/tree/master/lambdas) in the Untyped Lambda Calculus.

````
.\bin\untyped\Untyped prelude.lmbd boolean.lmbd tuple.lmbd numbers.lmbd numbers.lmbd lists.lmbd tests\cond.lmbd tests\numbers.lmbd tests\lists.lmbd -c "(λx. λy. y x) (λx. x) (lambda x.x)" -l -i .\lambdas

````

`-c "(λx. λy. y x) (λx. x) (lambda x.x)"` does nothing more than demonstrate submitting a λ-abstraction from the console after the abstractions 
in the files have evaluated. To submit multiple abstractions be sure to `;` separate them.

See [console app documentation](index.html) for all parameter options.

Evaluatining abstractions in the Untyped Recursive Lambda Calculus.

````
.\bin\untypedrecurs\UntypedRecurs prelude.lmbd boolean.lmbd tuple.lmbd numbers.lmbd numbers.lmbd lists.lmbd tests\cond.lmbd tests\numbers.lmbd tests\lists.lmbd tests\recursion.lmbd -c "(λx. λy. y x) (λx. x) (lambda x.x)" -l -i .\lambdas
````

Did you know you can write unit tests in the Lambda Calculus? Sure beats desk-checking the abstractions you compose. 

*)
