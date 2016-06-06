{-|
Module      : Control.Flower.Monad
Description : Combinators for directional monadic mapping
-}
module Control.Flower.Monad (
  (<<$), ($>>),
  (=<<$), ($>>=)
) where

{- $setup
>>> import Control.Flower.Compose

>>> let x = Just 3
>>> let y = Just 4

>>> let f = (+2)
>>> let g = (*2)
>>> let h = (+)
-}

{-| A left-associative operator alias for 'Control.Monad.mapM'

>>> putStrLn <. show <<$ [1,2,3]
1
2
3

-}
infixl 4 <<$
(<<$) :: (Foldable f, Monad m) => (a -> m b) -> f a -> m ()
(<<$) = mapM_

{-| An operator alias for 'Control.Monad.mapM'

>>> [1,2,3] $>> show .> putStrLn
1
2
3

-}
infixr 4 $>>
($>>) :: (Foldable f, Monad m) => f a -> (a -> m b) -> m ()
($>>) = flip mapM_

{-| A left-associative operator alias for 'Control.Monad.mapM'

>>> (\x -> [x+1]) =<<$ [1,2,3]
[[2,3,4]]

-}
infixl 4 =<<$
(=<<$) :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
(=<<$) = mapM

{-| An operator alias for 'Control.Monad.mapM'

>>> [1,2,3] $>>= \x -> [x+1]
[[2,3,4]]

-}
infixr 4 $>>=
($>>=) :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
($>>=) = flip mapM
