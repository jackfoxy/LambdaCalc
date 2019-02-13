namespace Jackfoxy.LambdaCalc

module Continuation =
    /// https://stackoverflow.com/questions/40052256/how-does-continuation-monad-really-work/42062682#42062682
    /// A continuation is a function that represents "the rest of the computation".
    type Cont<'T, 'U> = ('T -> 'U)

    /// An incomplete computation is a function which, when given a continuation,
    /// will return a value.
    type Inc<'T, 'U> = Cont<'T, 'U> -> 'U

    /// Creates an incomplete computation that holds the given value.
    let ret (t : 'T) : Inc<'T, _> =
        fun (cont : Cont<'T, _>) -> cont t

    /// Composition of incomplete computations.
    let bind (incT : Inc<'T, _>) (wrap : 'T -> Inc<'U, _>) : Inc<'U, _> =
        fun (contU : Cont<'U, _>) ->   // return an Inc, which is a function that takes a continuation as input
            incT (fun t ->             // force the given incomplete computation to cough up its wrapped value
                (wrap t) contU)        // re-wrap the raw value so it can be sent to the given continuation

    /// Monad definition.
    type ContinuationBuilder() =
        member __.Bind(inc, wrap) = bind inc wrap
        member __.Return(value) = ret value

    //let continuation = ContinuationBuilder()

