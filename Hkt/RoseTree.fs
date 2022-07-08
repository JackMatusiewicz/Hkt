namespace Hkt

type RoseTreeBrand = class end

type 'a RoseTree =
    | Leaf of 'a
    | Branch of 'a RoseTree list
    interface Application<RoseTreeBrand, 'a>

module RoseTree =

    let rec map (f : 'a -> 'b) (bt : 'a RoseTree) : 'b RoseTree =
        match bt with
        | Leaf a -> f a |> Leaf
        | Branch xs ->
            List.map (map f) xs |> Branch

type RoseTreeFunctor () =
    interface Functor<RoseTreeBrand> with
        member _.Map<'a, 'b> (x : Application<RoseTreeBrand, 'a>) (f : 'a -> 'b) =
            RoseTree.map f (x :?> 'a RoseTree) :> Application<RoseTreeBrand, 'b>