// Learn more about F# at http://fsharp.org

open Hkt

[<EntryPoint>]
let main argv =
    let xs = [ 1 .. 10 ]
    let result = Arrow.fold Arrow.rev [] xs
    printfn "%A" result
    0
