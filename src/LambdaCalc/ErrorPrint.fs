namespace Jackfoxy.LambdaCalc

module internal ErrorPrint =

    let mutable private buffer : string list = []

    let toBuffer s =
        buffer <- s::buffer

    let printInt (i : int) = 
        i.ToString()
        |> toBuffer

    let pr (s : string) =
        toBuffer s

    let printSpace() =
        toBuffer " "

    let flush() =

        buffer
        |> List.rev
        |> List.iter (fun x -> Microsoft.FSharp.Core.Printf.printf "%s" x)

        Microsoft.FSharp.Core.Printf.printf "%s" "\n"

        buffer <- []