open Hkt

[<EntryPoint>]
let main _ =
    let v = BinaryTree.Branch (BinaryTree.Leaf 1, 2, BinaryTree.Leaf 3)
    let u = RoseTree.Branch [RoseTree.Leaf 1 ; RoseTree.Leaf 2 ; RoseTree.Leaf 3]

    printfn "%A" <| Functor.tupleUp (BinaryTreeFunctor ()) v
    printfn "%A" <| Functor.tupleUp (RoseTreeFunctor ()) u
    0
