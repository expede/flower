{-# LANGUAGE NoImplicitPrelude #-}

module Control.Flower.Applicative.Lazy (
  ap,
  (<*), (*>),
  (<$*), (*$>),
  (<$**), (**$>)
) where

import Prelude (flip)
import Control.Applicative hiding ((<*), (*>))

ap :: Applicative f => f (a -> b) -> f a -> f b
ap = (<*>)

infixl 4 <*
(<*) :: Applicative f => f (a -> b) -> f a -> f b
(<*) = ap

infixl 4 *>
(*>) :: Applicative f => f a -> f (a -> b) -> f b
(*>) = flip ap

infixl 4 <$*
(<$*) :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
(<$*) = liftA2 -- AKA f <$ a <* b

infixl 4 *$>
(*$>) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
(*$>) = flip liftA2

infixl 4 <$**
(<$**) :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
(<$**) = liftA3 -- AKA f <$ a <* b <* c

infixl 4 **$>
(**$>) :: Applicative f => f a -> (a -> b -> c -> d) -> f b -> f c -> f d
(**$>) = flip liftA3
