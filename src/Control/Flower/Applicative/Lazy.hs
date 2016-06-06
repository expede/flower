{-|
Module      : Control.Flower.Applicative.Lazy
Description : Combinators for directional lazy applicative functors
-}

module Control.Flower.Applicative.Lazy (
  ap,
  lift2, lift3,
  (<*), (*>),
  (<$*), (*$>),
  (<$**), (**$>)
) where

import Prelude hiding ((<*), (*>))
import Control.Applicative hiding ((<*), (*>))

{- $setup
>>> import Control.Flower.Apply

>>> let x = Just 3
>>> let y = Just 4

>>> let f = (+2)
>>> let g = (*2)
>>> let h = (+)
-}

{-| A simple alias for 'Prelude.<*>'

>>> ap (Just (+1)) (Just 4)
Just 5

-}
ap :: Applicative f => f (a -> b) -> f a -> f b
ap = (<*>)

{-| Right-associative, left-flowing applicative operator

>>> Just (+1) <* Just 4
Just 5

-}
infixr 4 <*
(<*) :: Applicative f => f (a -> b) -> f a -> f b
(<*) = ap

{-| Left-associative, right-flowing applicative operator

>>> Just 4 *> Just (+1)
Just 5

-}
infixl 4 *>
(*>) :: Applicative f => f a -> f (a -> b) -> f b
(*>) = flip ap

{-| An alias for 'Control.Applicative.lift2', updating with unified "lift" naming

>>> lift2 (+) (Just 4) (Just 1)
Just 5

-}
lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 = liftA2

{-| Right-associative, left-flowing 'lift2' operator

>>> (+) <$* Just 4 |< Just 1
Just 5

-}
infixr 4 <$*
(<$*) :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
(<$*) = lift2

{-| Left-associative, right-flowing 'lift2' operator

>>> Just 4 >| Just 1 *$> (+)
Just 5

-}
infixl 4 *$>
(*$>) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
(*$>) = flip lift2

{-| An alias for 'Control.Applicative.lift3', updating with unified "lift" naming

>>> lift3 (\x y z -> x * y * z) (Just 4) (Just 3) (Just 2)
Just 24

-}
lift3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 = liftA3

{-| Right-associative, left-flowing 'lift3' operator

>>> (\x y z -> x * y * z) <$** Just 4 |< Just 3 |< Just 2
Just 24

-}
infixr 4 <$**
(<$**) :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
f <$** x = \y z -> lift3 f x y z -- AKA f <$ a <* b <* c

{-| Left-associative, right-flowing 'lift3' operator

>>> Just 2 >| Just 3 >| Just 4 **$> \x y z -> x * y * z
Just 24

-}
infixl 4 **$>
(**$>) :: Applicative f => f a -> (a -> b -> c -> d) -> f b -> f c -> f d
x **$> f = \y z -> lift3 f x y z
