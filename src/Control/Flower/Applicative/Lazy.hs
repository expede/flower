{-# LANGUAGE NoImplicitPrelude #-}

module Control.Flower.Applicative.Lazy (
  ap,
  (<*), (*>),
  (<<*), (*>>),
  (<$*), (*$>),
  (<$**), (**$>)
) where

import Prelude (flip)
import Control.Applicative hiding ((<*), (*>))

infixl 4 <*, *>, <<*, *>>, <$*, *$>

ap :: Applicative f => f (a -> b) -> f a -> f b
ap = (<*>)

(<*) :: Applicative f => f (a -> b) -> f a -> f b
(<*) = ap

(*>) :: Applicative f => f a -> f (a -> b) -> f b
(*>) = flip ap

(<<*) :: Applicative f => (a -> b) -> f a -> f b
(<<*) = liftA

(*>>) :: Applicative f =>  f a -> (a -> b) -> f b
(*>>) = flip liftA

(<$*) :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
(<$*) = liftA2 -- AKA f <$ a <* b

(*$>) :: Applicative f => f a -> (a -> b -> c) -> f b -> f c
(*$>) = flip liftA2

(<$**) :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
(<$**) = liftA3 -- AKA f <$ a <* b <* c

(**$>) :: Applicative f => f a -> (a -> b -> c -> d) -> f b -> f c -> f d
(**$>) = flip liftA3
