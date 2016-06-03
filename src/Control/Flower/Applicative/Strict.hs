{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict #-}

module Control.Flower.Applicative.Strict (
  ap',
  (<!*), (*!>),
  (<!$*), (*$!>)
) where

import Prelude (flip)
import Control.Applicative hiding ((<*), (*>))

ap' :: Applicative f => f (a -> b) -> f a -> f b
ap' = (<*>)

infixl 4 <!*
(<!*) :: Applicative f => f (a -> b) -> f a -> f b
(<!*) = ap'

infixl 4 *!>
(*!>) :: Applicative f => f a -> f (a -> b) -> f b
(*!>) = flip ap'

infixl 4 <!$*
(<!$*) :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
f <!$* a = \b -> liftA2 f a b -- AKA f <$ a <* b

infixl 4 *$!>
(*$!>) :: Applicative f => f a -> f b -> (a -> b -> c) -> f c
a *$!> b = \f -> liftA2 f a b -- AKA a *> b $> f
