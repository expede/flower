{-|
Module      : Control.Flower.Apply.Lazy
Description : Combinators for directional lazy application
-}

module Control.Flower.Apply.Lazy (
  (<|), (|>),
  (|<), (>|)
) where

{- $setup
>>> let x = 3
>>> let y = 4

>>> let f = (+2)
>>> let g = (*2)
>>> let h = (+)
-}

{-| Left-flowing application, equivalent to 'prelude.$'.

Read as "backwards application", "pipe from", or "pull from".

>>> (f <| x) == f x
True

>>> (g <| f <| x) == g (f x)
True

This operator can be chained together to show the dataflow through a series of functions

>>> negate <| succ <| 3 :: Int
-4

-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

{-| Right-flowing application, equivalent to 'prelude.$'.

Read as "forwards application" or "pipe into".

>>> (x |> f) == f x
True

>>> (x |> f |> g) == g (f x)
True

This operator can be chained together to show the dataflow through a series of functions

>>> 3 |> succ |> negate :: Int
-4

-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

{-| Left-flowing, left-associative application

Read as "pipe into the result of".
It may seem odd in trivial cases, but is useful for functions that take more than one argument,
as it will partially apply arguments one at a time.

>>> (f |< x) == f x
True

>>> (h |< y |< x) == ((h <| y) <| x)
True

Can be chained together to show the dataflow through a series of functions

>>> (+) |< 3 |< 5 :: Int
8

-}
infixl 0 |<
(|<) :: (a -> b) -> a -> b
(|<) = (<|)

{-| Right-flowing, right-associative application

Read as "pipe from the result of", or "pull from the result from".
It may seem odd in trivial cases, but is useful for functions that take more than one argument.

>>> (x >| f) == f x
True

>>> (x >| y >| h) == (x |> (y |> h))
True

Can be chained together to show the dataflow through a series of functions

>>> 3 >| 5 >| (+)
8

-}
infixr 0 >|
(>|) :: a -> (a -> b) -> b
(>|) = (|>)
