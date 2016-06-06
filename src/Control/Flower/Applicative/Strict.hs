{-|
Module      : Control.Flower.Applicative.Strict
Description : Combinators for directional strict applicative functors
-}

module Control.Flower.Applicative.Strict (
  lift2',
  (<!$*), (*$!>),
  (<!$**), (**$!>)
) where

import Prelude hiding ((<*), (*>))
import Control.Flower.Functor.Strict
import Control.Flower.Applicative.Lazy

{- $setup
>>> import Control.Flower.Apply

>>> let x = Just 3
>>> let y = Just 4

>>> let f = (+2)
>>> let g = (*2)
>>> let h = (+)
-}

{-| An alias for 'Control.Applicative.lift2', updating with unified "lift" naming

>>> lift2' (+) (Just 4) (Just 1)
Just 5

-}
lift2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2' f a b = (f <!$ a) <* b

{-| Right-associative, left-flowing `lift2'` operator

>>> (+) <!$* Just 4 |< Just 1
Just 5

-}
infixr 4 <!$*
(<!$*) :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f <!$* a = \b -> lift2' f a b

{-| Left-associative, right-flowing `lift2'` operator

>>> Just 1 >| Just 4 *$!> (+)
Just 5

-}
infixl 4 *$!>
(*$!>) :: Monad m => m a -> (a -> b -> c) -> m b -> m c
a *$!> f = \b -> lift2' f a b

{-| A strict version of 'Control.Flower.Applicative.Lazy.lift3'

>>> lift3' (\x y z -> x * y * z) (Just 4) (Just 3) (Just 2)
Just 24

-}
lift3' :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3' f a b c = ((f <!$ a) <* b) <* c

{-| Right-associative, left-flowing `lift3'` operator

>>> (\x y z -> x * y * z) <!$** Just 4 |< Just 3 |< Just 2
Just 24

-}
infixr 4 <!$**
(<!$**) :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f <!$** x = \y z -> lift3' f x y z -- AKA f <$ a <* b <* c

{-| Left-associative, right-flowing `lift3'` operator

>>> Just 2 >| Just 3 >| Just 4 **$!> \x y z -> x * y * z
Just 24

-}
infixl 4 **$!>
(**$!>) :: Monad m => m a -> (a -> b -> c -> d) -> m b -> m c -> m d
x **$!> f = \y z -> lift3' f x y z
