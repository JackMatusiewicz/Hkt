namespace Hkt

type FreeMonadBrand = class end

// A type representing the free monad.
type Free<'f, 'a, 'x when 'x :> Application<'f, 'a>> =
    | Pure of 'a
    | Join of Application<'f, Free<'f, 'a, 'x>>    

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
