namespace Hkt

open TypeEquality

// A GADT that represents a defunctionalised set of functions.
type Arrow<'a,'b> =
    | Add of Teq<'a, int> * Teq<'b, int>
    | Cons of Teq<'a, int> * Teq<'b, int list>
    | Rev of Teq<'a, int> * Teq<'b, int list>

[<RequireQualifiedAccess>]
module Arrow =

    let add : Arrow<int, int> = Add (Teq.refl<int>, Teq.refl<int>)
    let cons : Arrow<int, int list> = Cons (Teq.refl<int>, Teq.refl<int list>)
    let rev : Arrow<int, int list> = Cons (Teq.refl<int>, Teq.refl<int list>)

    let private constructFunc<'a, 'b, 'c, 'd>
        (dom : Teq<'a, 'b>)
        (range : Teq<'c, 'd>)
        : Teq<'a * 'c -> 'c, 'b * 'd -> 'd>
        =
        let tupled = Teq.Cong.pair dom range
        Teq.Cong.func tupled range

    let private apply<'a, 'b> (ar : Arrow<'a, 'b>) : ('a * 'b) -> 'b =
        match ar with
        | Add (dom, range) ->
            fun (a, b) -> a + b
            |> Teq.castFrom (constructFunc dom range)
        | Cons (dom, range) ->
            fun (a, b) -> a :: b
            |> Teq.castFrom (constructFunc dom range)
        | Rev (dom, range) ->
            fun (a, b) -> List.append b [a]
            |> Teq.castFrom (constructFunc dom range)

    let fold<'a,'b> (ar : Arrow<'a, 'b>) (state : 'b) (xs : 'a list) : 'b =
        let func = apply ar
        List.fold (fun b a -> func (a,b)) state xs
