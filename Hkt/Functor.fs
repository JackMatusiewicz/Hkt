namespace Hkt

module Functor =

    /// A simple example showing how we can use this implementation to write
    /// higher-kinded polymorphic functions in F#.
    let tupleUp (f : Functor<'f>) (v : Application<'f, 'a>) : Application<'f, 'a * 'a> =
        f.Map v (fun a -> (a,a))