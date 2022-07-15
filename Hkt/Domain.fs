namespace Hkt

/// Represents a higher-kinded type 'f<'a>
type Application<'f, 'a> = interface end

type Functor<'f> =
    abstract Map<'a, 'b> : Application<'f, 'a> -> ('a -> 'b) -> Application<'f, 'b>

type Monad<'f> =
    inherit Functor<'f>
    abstract Pure<'a> : 'a -> Application<'f, 'a>
    abstract Bind<'a, 'b> :
        ('a -> Application<'f, 'b>)
        -> Application<'f, 'a>
        -> Application<'f, 'b>