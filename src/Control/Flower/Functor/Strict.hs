{-# LANGUAGE Strict #-}

module Control.Flower.Functor.Strict (lift', (<!$), ($!>)) where

lift' :: Functor f => (a -> b) -> f a -> f b
lift' = fmap

(<!$) :: Functor f => (a -> b) -> f a -> f b
(<!$) = lift'

($!>) :: Functor f => f a -> (a -> b) -> f b
($!>) = flip lift'
