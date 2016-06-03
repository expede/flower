{-# LANGUAGE Strict #-}

module Control.Flower.Functor.Strict (lift', over', (<!$), ($!>)) where

import Flow ((<!))

-- $setup
-- >>> import Flow ((<|))

{- | A strict version of `lift`

>>> lift' (+1) <| Just 0
Just 1

>>> lift' (lift' (+1)) [[1,2,3],[4,5,6]]
[[2,3,4],[5,6,7]]

-}
lift' :: Functor f => (a -> b) -> f a -> f b
lift' = fmap

{- | Alias for `apply'`, for readability (especially when teaching)

>>> lift' (+1) `over'` Just 0
Just 1

-}
over' :: (a -> b) -> a -> b
over' = (<!)

infixl 4 <!$
{- | Operator for `lift'` highlighting the direction of data flow

>>> (+1) <!$ Just 0
Just 1

-}
(<!$) :: Functor f => (a -> b) -> f a -> f b
(<!$) = lift'

infixl 4 $!>
{- | Operator for `lift'` highlighting the reversed direction of data flow

>>> Just 0 $!> (+1)
Just 1

-}
($!>) :: Functor f => f a -> (a -> b) -> f b
($!>) = flip lift'
