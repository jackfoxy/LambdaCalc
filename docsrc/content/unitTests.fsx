(**
Unit Tests
==========

Did you know there is a λ-abstraction you can use as the `assert` statement in unit tests? It is the boolean function on two variables
for equivalence, sometimes known as `iff` or [xnor](https://en.wikipedia.org/wiki/XNOR_gate).

We have implemented this function abstraction as [xnor](https://github.com/jackfoxy/LambdaCalc/blob/master/lambdas/boolean.lmbd#L9), 
and possibly not in the most efficient manner. Try your hand at developing more efficient implementations. 
You have unit tests to help you.

There are a number of example unit tests [here](https://github.com/jackfoxy/LambdaCalc/tree/master/lambdas/tests).

The tests make use of a very minimalistic language defined in the untyped lambda calculus (list is in dependency order):

- [prelude](https://github.com/jackfoxy/LambdaCalc/blob/master/lambdas/prelude.lmbd) includes the `bottom` and `fix` abstraction 
values required by the [untyped recursive lambda calculus](operationalSemantics.html).
- [boolean](https://github.com/jackfoxy/LambdaCalc/blob/master/lambdas/boolean.lmbd) includes the boolean abstractions and values for the most common boolean operations.
- [tuple](https://github.com/jackfoxy/LambdaCalc/blob/master/lambdas/tuple.lmbd) the pair tuple and operations.
- [numbers](https://github.com/jackfoxy/LambdaCalc/blob/master/lambdas/numbers.lmbd) some natural number abstractions and operations on natural numbers.
- [lists](https://github.com/jackfoxy/LambdaCalc/blob/master/lambdas/lists.lmbd) the singly linked list abstraction.

Most of the language elements have been described by [Pierce](http://www.cis.upenn.edu/~bcpierce/tapl/) and elsewhere, with the `list` terms being a unique implementation 
(that is not to say no one else has done it before) implementing `nil` as `bottom` in order to function with `fix` in the 
[untyped recursive lambda calculus](operationalSemantics.html).

*)
