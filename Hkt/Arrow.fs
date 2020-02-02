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

    let apply<'a, 'b> (ar : Arrow<'a, 'b>) : ('a * 'b) -> 'b =
        match ar with
        | Add (dom, range) ->
            let tupled = Teq.Cong.pair dom range
            let funcTeq = Teq.Cong.func tupled range
            fun (a, b) -> a + b
            |> Teq.castFrom funcTeq
        | Cons (dom, range) ->
            let tupled = Teq.Cong.pair dom range
            let funcTeq = Teq.Cong.func tupled range
            fun (a, b) -> a :: b
            |> Teq.castFrom funcTeq
        | Rev (dom, range) ->
            let tupled = Teq.Cong.pair dom range
            let funcTeq = Teq.Cong.func tupled range
            fun (a, b) -> List.append b [a]
            |> Teq.castFrom funcTeq

    let fold<'a,'b> (ar : Arrow<'a, 'b>) (state : 'b) (xs : 'a list) : 'b =
        let func = apply ar
        List.fold (fun b a -> func (a,b)) state xs
