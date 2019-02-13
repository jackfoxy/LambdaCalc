namespace Jackfoxy.LambdaCalc

module Continuation =
    type Cont<'T, 'U> = 'T -> 'U

    type Inc<'T, 'U> = Cont<'T, 'U> -> 'U

    type ContinuationBuilder =
        new  : unit -> ContinuationBuilder
        member Bind : inc : Inc<'T, 'U> * wrap : ('T -> Inc<'V, 'U>) -> Inc<'V, 'U>
        member Return : value : 'T -> Inc<'T, 'U>
        