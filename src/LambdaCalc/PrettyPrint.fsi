namespace Jackfoxy.LambdaCalc

open Common

module PrettyPrint =

    val mutable useLambda : bool

    val pr: string -> unit

    val printInt : i : int -> unit

    val printSpace : unit -> unit

    val flush : unit -> unit

    val printComments : term : Term -> commentLines : CommentLine list -> CommentLine list

    val printInputSource : term : Term -> inputLines : InputLines list -> unit