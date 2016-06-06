module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Control/Flower/Apply/Lazy.hs"
               , "src/Control/Flower/Apply/Strict.hs"

               , "src/Control/Flower/Compose.hs"

               , "src/Control/Flower/Functor/Lazy.hs"
               , "src/Control/Flower/Functor/Strict.hs"

               , "src/Control/Flower/Applicative/Lazy.hs"
               , "src/Control/Flower/Applicative/Strict.hs"

               , "src/Control/Flower/Monad.hs"
               ]
