(**
<style>.adsf{padding: 2px 4px;color: #d14;background-color: #f7f7f9;border: 1px solid #e1e1e8;font-family: Monaco,Menlo,Consolas,"Courier New",monospace;font-size: 12px;-moz-border-radius: 3px;border-radius: 3px;}</style>
Operational Semantics
=====================

Operational Semantics specify how a language behaves. The Lambda Calculus specification, in modified [Baukus-Naur form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form), 
is from Pierce's [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/), p. 72.


Untyped Lambda Calculus
-----------------------

**Syntax**

The legal forms of the language, terms, are either a variable, abstraction, or abstraction application.
````
t ::=
    x       /* variable */
    λx.t    /* abstraction */
    t t     /* application */
````
Values may represent abstractions.

````
v ::=
    λx.t    /* abstraction value */
````

**Evaluation**

The horizontal line signifies an IF / THEN rule. 

`↦` implies evaluates in one step.

<span class="adsf">[x ↦ v<sub>2</sub>]t<sub>12</sub></span> reads substitute <span class="adsf">v<sub>2</sub></span> for all occurences of free variable <span class="adsf">x</span> in term <span class="adsf">t<sub>12</sub></span> 
("free" meaning the variable is not bound by an enclosing abstraction).

````
       t1 &2B62; t1'    
    _______________      /* E-APP1 */
    t1 t2 &#2B62; t1' t2

       t2 &#2B62; t2'    
    _______________      /* E-APP2 */
    v1 t2 &#2B62; v1 t2'

    (λx.t12) v2 &#2B62; [x &#21B6; v2]t12    /* E-APPABS */
````

`E-APP1` and `E-APP2` are congruence rules. 

`E-APPABS` is the computation or substitution rule. Note the rule matches
when `v<sub>2</sub>` ranges over values. Thus the right-hand side must be evaluated and reduced first, and so this rule 
controls the order of evaluation and is the crux of by-value evaluation. This is problematic for evaluating recursion, 
which the following extension to the semantics addresses.

Untyped Recursive Lambda Calculus
---------------------------------

This operational semantics adds one substitution rule to the evaluation operational semantics.

`⊢` indicates the right-hand side is in the context of the left-hand side.

`f: (λf. λx. _)` reads `f` is an abstraction of the form `λf. λx. _`. Note the recursion. The form of the abstraction body is unimportant.

````
f: (λf. λx. _) &#22A6; (λy. (λx. f (λy. x x y)) (λx. f (λy. x x y)) y) (λt. λb. b)
_____________________________________________________________________________
                                   λb. b
````
*)
