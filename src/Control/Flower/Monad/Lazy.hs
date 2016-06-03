module Control.Flower.Monad.Lazy (
  (=<<$), ($>>=),
  (=<<<$), ($>>>=),
  (=<<<<$), ($>>>>=),
  (=<<<<<$), ($>>>>>=)
) where

import Control.Monad

infixl 4 $>>=
($>>=)  :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
($>>=) = liftM2

infixl 4 =<<$
(=<<$) :: (Monad m) => m a -> (a -> b -> r) -> m b -> m r
(=<<$) = flip liftM2

infixl 4 =<<<$
(=<<<$) :: (Monad m) => (a -> b -> c -> r) -> m a -> m b -> m c -> m r
(=<<<$) = liftM3

infixl 4 $>>>=
($>>>=) :: (Monad m) => m a -> (a -> b -> c -> r) -> m b -> m c -> m r
($>>>=) = flip liftM3

infixl 4 =<<<<$
(=<<<<$) :: (Monad m) => (a -> b -> c -> d -> r) -> m a -> m b -> m c -> m d -> m r
(=<<<<$) = liftM4

infixl 4 $>>>>=
($>>>>=) :: (Monad m) => m a -> (a -> b -> c -> d -> r) -> m b -> m c -> m d -> m r
($>>>>=) = flip liftM4

infixl 4 =<<<<<$
(=<<<<<$) :: (Monad m) => (a -> b -> c -> d -> e -> r) -> m a -> m b -> m c -> m d -> m e -> m r
(=<<<<<$) = liftM5

infixl 4 $>>>>>=
($>>>>>=) :: (Monad m) => m a -> (a -> b -> c -> d -> e -> r) -> m b -> m c -> m d -> m e -> m r
($>>>>>=) = flip liftM5
