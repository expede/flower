{-# LANGUAGE Strict #-}

module Control.Flower.Functor.Strict (lift', (<!$), ($!>)) where

lift' :: Functor f => (a -> b) -> f a -> f b
lift' = fmap

infixl 4 <!$
(<!$) :: Functor f => (a -> b) -> f a -> f b
(<!$) = lift'

infixl 4 $!>
($!>) :: Functor f => f a -> (a -> b) -> f b
($!>) = flip lift'
