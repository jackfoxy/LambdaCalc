namespace Jackfoxy.LambdaCalc

open Support.Error

/// A variable, abstraction, or application of abstraction in the lambda calculus.
type Term =
    /// A variable in the lambda calculus.
    | Variable of FileInfo : Info * DeBruinIndex : int * ContextLength : int
    /// An abstraction, or lambda, in the lambda calculus.
    | Abstraction of FileInfo : Info * Name : string * Abstraction: Term
    /// An application of abstraction in the lambda calculus.
    | Application of FileInfo : Info * Applicator : Term * Applicand : Term

/// Assign a value to a symbol.
type Binding = 
    /// Bind to a name.
    | NameBind 
    /// Assign a term to a lambda abstraction
    | AbstractionBind of Term

/// Actions on parsed term.
type Command =
    /// Evaluate the parsed term.
    | Eval of FileInfo : Info * Term
    /// Bind the parsed term.
    | Bind of FileInfo : Info * Name : string * Binding

/// Context is the list of bindings in the current state.
type Context = (string * Binding) list

/// Syntax trees and associated context management functions.
module CommonAst =

    /// A context with no bindings.
    val emptyContext : Context

    /// Count of bindings in current state (context).
    val ctxLength : context : Context -> int

    /// Add binding to the context.
    val addBinding : context : Context -> x : string -> binding : Binding -> Context

    /// Add a name binding to the context.
    val addName : context : Context -> name : string -> Context

    /// Choose a distinct variable name in the context.
    val pickfreshname : context : Context -> name : string -> Context * string

    /// Get context index by binding name.
    val name2Index : fileInfo : Info -> context : Context -> x : string-> int

    val termSubstTop : abstractionTerm : Term -> term : Term -> Term

    /// Lookup binding in context by index, shifting terms if it is an abstraction bindings.
    val getBinding : fileInfo : Info -> context : Context -> i : int -> Binding

    /// Retrieve file info for term.
    val termInfo : term : Term -> Info
