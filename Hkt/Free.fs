namespace Hkt

open Hkt

type FreeBrand<'f> = interface end

// A type representing the free monad.
type Free<'f, 'a, 'x when 'x :> Application<'f, 'a>> =
    | Pure of 'a
    | Join of Application<'f, Free<'f, 'a, 'x>>
    interface Application<FreeBrand<'f>, 'a>

module Free =

    let rec concatFree
        (functor : Functor<'f>)
        (free : Free<'f, Free<'f, 'a, Application<'f, 'a>>, Application<'f, Free<'f, 'a, Application<'f, 'a>>>>)
        : Free<'f, 'a, Application<'f, 'a>>
        =
        match free with
        | Pure a -> a
        | Join ffa ->
            functor.Map ffa (concatFree functor)
            |> Join

    /// How you lift your type into the Free Monad
    let liftFree
        (functor : Functor<'f>)
        (v : Application<'f, 'a>)
        : Free<'f, 'a, Application<'f, 'a>>
        =
        functor.Map v Pure
        |> Join

    /// How we pull values out from the Free Monad.
    let rec foldFree
        (functor : Functor<'f>)
        (fold : Application<'f, 'a> -> 'a)
        (v : Free<'f, 'a, Application<'f, 'a>>)
        : 'a
        =
        match v with
        | Pure a -> a
        | Join ffa ->
            functor.Map ffa (foldFree functor fold)
            |> fold

    let rec map
        (functor : Functor<'f>)
        (f : 'a -> 'b)
        (v : Free<'f, 'a, Application<'f, 'a>>)
        : Free<'f, 'b, Application<'f, 'b>>
        =
        match v with
        | Pure a -> f a |> Pure
        | Join ffa ->
            functor.Map ffa (map functor f)
            |> Join

    let bind<'f, 'a, 'b>
        (innerFunctor : Functor<'f>)
        (f : 'a -> Free<'f, 'b, Application<'f, 'b>>)
        (v : Free<'f, 'a, Application<'f, 'a>>)
        : Free<'f, 'b, Application<'f, 'b>>
        =
        concatFree innerFunctor (map innerFunctor f v)

type FreeFunctor<'f, 'free when 'free :> FreeBrand<'f>>(fFunctor : Functor<'f>) =
    interface Functor<'free> with
        member this.Map<'a, 'b> v (f : 'a -> 'b) =
            let v = v
                :?> Application<FreeBrand<'f>, 'a>
                :?> Free<'f, 'a, Application<'f, 'a>>

            Free.map fFunctor f v
            :> Application<FreeBrand<'f>, 'b>
            :?> Application<'free, 'b>

type FreeMonad<'f, 'free when 'free :> FreeBrand<'f>> (fFunctor : Functor<'f>) =
    // I believe this is how the implementation of typeclass inheritance is in Haskell.
    // A child typeclass with have an instance of the parent's dictionary inside itself.
    let freeFunctor = FreeFunctor(fFunctor)

    interface Monad<'free> with
        member _.Bind<'a, 'b> (f : 'a -> Application<'free, 'b>) v =
            Free.bind
                fFunctor
                (fun v ->
                    f v
                    :?> Application<FreeBrand<'f>, 'b>
                    :?> Free<'f, 'b, Application<'f, 'b>>
                )
                (v :?> Application<FreeBrand<'f>, 'a> :?> Free<'f, 'a, Application<'f, 'a>>)
            :> Application<FreeBrand<'f>, 'b>
            :?> Application<'free, 'b>

        member _.Map a b = (freeFunctor :> Functor<'free>).Map a b

        member _.Pure a =
            Free.Pure a
            :> Application<FreeBrand<'f>, 'a>
            :?> Application<'free, 'a>