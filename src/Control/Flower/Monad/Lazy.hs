module Control.Flower.Monad.Lazy (
  (=<$), ($>=),
  (=<<$), ($>>=),
  (=<<<$), ($>>>=),
  (=<<<<$), ($>>>>=),
  (=<<<<<$), ($>>>>>=)
) where

import Control.Monad

(=<$) :: (Monad m) => (a -> r)
                  -> m a
                  -> m r
(=<$) = liftM

($>=) :: (Monad m) => m a
                  -> (a -> r)
                  -> m r
($>=) = flip liftM

($>>=)  :: (Monad m) => (a -> b -> r)
                    -> m a
                    -> m b
                    -> m r
($>>=) = liftM2

(=<<$) :: (Monad m) => m a
                   -> (a -> b -> r)
                   -> m b
                   -> m r
(=<<$) = flip liftM2

(=<<<$) :: (Monad m) => (a -> b -> c -> r)
                    -> m a
                    -> m b
                    -> m c
                    -> m r
(=<<<$) = liftM3

($>>>=) :: (Monad m) => m a
                    -> (a -> b -> c -> r)
                    -> m b
                    -> m c
                    -> m r
($>>>=) = flip liftM3

(=<<<<$) :: (Monad m) => (a -> b -> c -> d -> r)
                     -> m a
                     -> m b
                     -> m c
                     -> m d
                     -> m r
(=<<<<$) = liftM4

($>>>>=) :: (Monad m) => m a
                     -> (a -> b -> c -> d -> r)
                     -> m b
                     -> m c
                     -> m d
                     -> m r
($>>>>=) = flip liftM4

(=<<<<<$) :: (Monad m) => (a -> b -> c -> d -> e -> r)
                     -> m a
                     -> m b
                     -> m c
                     -> m d
                     -> m e
                     -> m r
(=<<<<<$) = liftM5

($>>>>>=) :: (Monad m) => m a
                     ->  (a -> b -> c -> d -> e -> r)
                     -> m b
                     -> m c
                     -> m d
                     -> m e
                     -> m r
($>>>>>=) = flip liftM5
