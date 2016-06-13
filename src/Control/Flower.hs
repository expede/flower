{-|
Module      : Control.Flower
Description : Modern, readable, directional Haskell

== Use

>>> import Control.Flower

== Rationale

> Mathematics, rightly viewed, possesses not only truth,
> but supreme beauty -- a beauty cold and austere, like that of sculpture,
> without appeal to any part of our weaker nature, without the gorgeous
> trappings of painting or music, yet sublimely pure, and capable of a stern
> perfection such as only the greatest art can show. The true spirit of delight,
> the exaltation, the sense of being more than Man, which is the touchstone of
> the highest excellence, is to be found in mathematics as surely as poetry.
> - Bertrand Russell, "The Study of Mathematics"

Inspired by the wonderful @Flow@ package, Flower provides directional operators
for many common Haskell functions.

With the pipe operator ('|>') proliferating through OCaml, F#, and Elixir,
it's becoming clear which way the wind is blowing. A dataflow model is very
natural to functional programming.

Thinking in Haskell is multidimensional, reading forwards and backwards,
and through levels of abstraction. This is extremely powerful, but does introduce
a leaning curve (in grade school, when starting with Haskell, or both).

Here, instead of 'Prelude.$', we use '<|', or reversed with '|>'. Instead of
'Prelude.<$>', we use '<$', and reversed '$>'. Many of the combinators are
built up from meaningful character combinations. One such example is 'Prelude.lift2',
which is translated into '<$**'. '<$**', as 'f <$ a <* b <* c'.

Please do note that 'Control.Flower' exposes conflicting combinators versus the
standard 'Prelude'.

=== Teaching
Teaching concepts becomes simplified by providing a visual aid. Many of the operators
are made up of simpler symbols, much in the same way as the @Lens@ library.

One common challenge when teaching Haskell is showing what an applicative
or monad "mean". By using a progressive, modular picture of each abstraction,
we help build the intuition.

=== Reading
A focus on a single direction of data flow makes code easy to follow.

=== Simplify
All `lift`s (`fmap`, `liftA*` and `liftM*`) are unified as `lift*`.

-}
module Control.Flower (
  -- * Basic data flow
  module Control.Flower.Apply,
  module Control.Flower.Compose,

  -- * Functors
  module Control.Flower.Functor,
  module Control.Flower.Applicative,
  module Control.Flower.Monad
) where

import Control.Flower.Apply
import Control.Flower.Compose
import Control.Flower.Functor
import Control.Flower.Applicative
import Control.Flower.Monad
