{-|
Module      : Control.Flower.Apply.Strict
Description : Combinators for directional lazy application
-}
module Control.Flower.Apply.Strict (
  (<!), (!>),
  (!<), (>!)
) where

{- $setup
>>> let x = 3
>>> let y = 4

>>> let f = (+2)
>>> let g = (*2)
>>> let h = (+)
-}


{-| Left-flowing strict application, equivalent to 'prelude.$'.

Read as "backwards strict application", "strict pipe from", or "strict pull from".

>>> (f <! x) == f x
True

>>> (g <! f <! x) == g (f x)
True

This operator can be chained together to show the dataflow through a series of functions

>>> negate <! succ <! 3 :: Int
-4

-}
infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = f $! x

{-| Right-flowing strict application, equivalent to 'prelude.$'.

Read as "forward strict application" or "strict pipe into".

>>> (x !> f) == f x
True

>>> (x !> f !> g) == g (f x)
True

This operator can be chained together to show the dataflow through a series of functions

>>> 3 !> succ !> negate :: Int
-4

-}
infixr  0 <!
(<!) :: (a -> b) -> a -> b
f <! x = f $! x

{-| Left-flowing, left-associative strict application

Read as "strictly pipe into the result of".
It may seem odd in trivial cases, but is useful for functions that take more than one argument,
as it will partially apply arguments one at a time.

>>> (f !< x) == f x
True

>>> (h !< y !< x) == ((h <! y) <! x)
True

Can be chained together to show the dataflow through a series of functions

>>> (+) !< 3 !< 5 :: Int
8

-}

infixl 1 !<
(!<) :: (a -> b) -> a -> b
(!<) = (<!)

{-| Right-flowing, right-associative strict application

Read as "strictly pipe from the result of", or "strictly pull from the result from".
It may seem odd in trivial cases, but is useful for functions that take more than one argument.

>>> (x >! f) == f x
True

>>> (x >! y >! h) == (x !> (y !> h))
True

Can be chained together to show the dataflow through a series of functions

>>> 3 >! 5 >! (+)
8

-}
infixr 1 >!
(>!) :: a -> (a -> b) -> b
(>!) = (!>)
