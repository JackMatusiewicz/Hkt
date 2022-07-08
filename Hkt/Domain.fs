namespace Hkt

/// Represents a higher-kinded type 'f<'a>
type Application<'f, 'a> = interface end

type Functor<'f> =
    abstract Map<'a, 'b> : Application<'f, 'a> -> ('a -> 'b) -> Application<'f, 'b>