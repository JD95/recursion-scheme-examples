{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cata where

import           Data.Functor.Foldable

map :: (a -> b) -> [a] -> [b]
map f = cata (g f)
  where
    g :: (a -> b) -> ListF a [b] -> [b]
    g _ Nil        = []
    g f (Cons a b) = (f a) : b

partitionOn :: Eq b => (a -> b) -> (b -> Bool) -> [a] -> ([a], [a])
partitionOn p t = cata (f t p)
  where
    f t p Nil = ([], [])
    f t p (Cons a (pass, fail))
      | t (p a) = (a : pass, fail)
      | otherwise = (pass, a : fail)
