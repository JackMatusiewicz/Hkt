namespace Hkt

type BinaryTreeBrand = class end

type 'a BinaryTree =
    | Leaf of 'a
    | Branch of 'a BinaryTree * 'a * 'a BinaryTree
    interface Application<BinaryTreeBrand, 'a>

module BinaryTree =

    let rec map (f : 'a -> 'b) (bt : 'a BinaryTree) : 'b BinaryTree =
        match bt with
        | Leaf a -> f a |> Leaf
        | Branch (l,v,r) ->
            Branch(map f l, f v, map f r)

type BinaryTreeFunctor () =
    interface Functor<BinaryTreeBrand> with
        member _.Map<'a, 'b> (x : Application<BinaryTreeBrand, 'a>) (f : 'a -> 'b) =
            BinaryTree.map f (x :?> 'a BinaryTree) :> Application<BinaryTreeBrand, 'b>