{-|
Module      : Control.Flower.Functor.Strict
Description : Strict functor combinators (requires a monad instance)
-}

module Control.Flower.Functor.Strict (
  lift',
  over',
  (<!$),
  ($!>)
) where

import Control.Flower.Apply ((<!))
import Control.Monad ((<$!>))

-- $setup
-- >>> import Flow ((<|))

{- | A strict version of `lift`

>>> lift' (+1) <| Just 0
Just 1

>>> lift' (lift' (+1)) [[1,2,3],[4,5,6]]
[[2,3,4],[5,6,7]]

-}
lift' :: Monad m => (a -> b) -> m a -> m b
lift' = (<$!>)

{- | Alias for `apply'`, for readability (especially when teaching)

>>> lift' (+1) `over'` Just 0
Just 1

-}
over' :: (a -> b) -> a -> b
over' = (<!)

{- | Operator for `lift'` highlighting the direction of data flow

>>> (+1) <!$ Just 0
Just 1

-}
infixr 4 <!$
(<!$) :: Monad f => (a -> b) -> f a -> f b
(<!$) = lift'

{- | Operator for `lift'` highlighting the reversed direction of data flow

>>> Just 0 $!> (+1)
Just 1

-}
infixl 4 $!>
($!>) :: Monad f => f a -> (a -> b) -> f b
($!>) = flip lift'
