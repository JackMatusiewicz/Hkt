open Hkt

[<EntryPoint>]
let main _ =
    let v = BinaryTree.Branch (BinaryTree.Leaf 1, 2, BinaryTree.Leaf 99)
    let freeV = Free.liftFree (BinaryTreeFunctor ()) v
    let sum =
        Free.foldFree
            (BinaryTreeFunctor ())
            (fun v ->
                v :?> int BinaryTree
                |> BinaryTree.fold id (fun a l r -> a + l + r)
            )
            freeV
    printfn "%d" sum
    0
