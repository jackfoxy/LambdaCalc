namespace Jackfoxy.LambdaCalc

open Common

/// Formatted term printing.
module PrettyPrint =

    val mutable useLambda : bool

    val pr: string -> unit

    val printInt : i : int -> unit

    val printSpace : unit -> unit

    val flush : unit -> unit

    val printComments : term : Term -> commentLines : CommentLine list -> inputLines : InputLines list -> CommentLine list * InputLines list

    val printRemainingComments : commentLines : CommentLine list -> unit