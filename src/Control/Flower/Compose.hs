{-|
Module      : Control.Flower.Compose
Description : Directional composition combinators
-}
module Control.Flower.Compose ((<.), (.>)) where

{- $setup
>>> import Control.Flower.Apply.Lazy

>>> let x = 3
>>> let y = 4

>>> let f = (+2)
>>> let g = (*2)
>>> let h = (+)
-}

{-| Right-flowing, left-associative composition

Note that this is the opposite direction from typical composition

>>> (f .> g) x == (g . f) x
True

Can be combined with application combinators

>>> 5 |> (+1) .> (*10) :: Int
60

-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
a .> b =  b . a

{-| Left-flowing, right-associative composition

>>> (g <. f) x == (g . f) x
True

Can be combined with application combinators

>>> (+1) <. (*10) <| 5 :: Int
51

-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> a -> c
b <. a = b . a
