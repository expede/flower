{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict #-}

module Control.Flower.Applicative.Strict (
  ap',
  (<!*), (*!>),
  (<<!*), (*!>>),
  (<!$*), (*$!>)
) where

import Prelude (flip)
import Control.Applicative hiding ((<*), (*>))

infixl 4 <!*, *!>, <<!*, *!>>, <!$*, *$!>

ap' :: Applicative f => f (a -> b) -> f a -> f b
ap' = (<*>)

(<!*) :: Applicative f => f (a -> b) -> f a -> f b
(<!*) = ap'

(*!>) :: Applicative f => f a -> f (a -> b) -> f b
(*!>) = flip ap'

(<<!*) :: Applicative f => (a -> b) -> f a -> f b
(<<!*) = liftA

(*!>>) :: Applicative f => f a -> (a -> b) -> f b
(*!>>) = flip liftA

(<!$*) :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
f <!$* a = \b -> liftA2 f a b -- AKA f <$ a <* b

(*$!>) :: Applicative f => f a -> f b -> (a -> b -> c) -> f c
a *$!> b = \f -> liftA2 f a b -- AKA a *> b $> f
