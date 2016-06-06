{-|
Module      : Control.Flower.Functor.Lazy
Description : Lazy functor combinators
-}

module Control.Flower.Functor.Lazy (lift, over, (<$), ($>)) where

import Prelude hiding ((<$))
import Flow ((<|))

-- $setup
-- >>> import Flow ((|>), (<.), (.>))

{-| Rename `fmap` to `lift` for consistency

>>> lift (+1) <| Just 0
Just 1

>>> lift (lift (+1)) [[1,2,3],[4,5,6]]
[[2,3,4],[5,6,7]]

-}
lift :: Functor f => (a -> b) -> f a -> f b
lift = fmap

{-| Alias for `apply`, for readability (especially when teaching)

>>> lift (+1) `over` Just 0
Just 1

>>> Just 0 $> (+1) $> (+2)
Just 3

>>> (*2) <$ ([1,2,3] $> (+1))
[4,6,8]

>>> (+8) .> (*2) <$ Just 0
Just 16

>>> (*3) <. (+8) <$ Just 0
Just 24

-}
over :: (a -> b) -> a -> b
over = (<|)

infixr 4 <$
{-| Operator for `lift` highlighting the direction of data flow

>>> (+1) <$ Just 0
Just 1

>>> (+2) <$ (+1) <$ Just 0
Just 3


>>> ((*2) <$ [1,2,3]) $> (+1)
[3,5,7]

>>> Just 0 $> (+8) .> (*2)
Just 16

>>> Just 0 $> (*3) <. (+8)
Just 24

-}
(<$) :: Functor f => (a -> b) -> f a -> f b
(<$) = lift

infixl 4 $>
{-| Operator for `lift` highlighting the reversed direction of data flow

>>> Just 0 $> (+1)
Just 1

-}
($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip lift
