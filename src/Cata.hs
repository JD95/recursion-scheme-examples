{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cata where

import Data.Functor.Foldable

mapF :: (a -> b) -> ListF a [b] -> [b]
mapF _ Nil = []
mapF f (Cons a b) = (f a) : b

map :: (a -> b) -> [a] -> [b]
map f = cata (mapF f)

partitionOn :: Eq b => (a -> b) -> (b -> Bool) -> [a] -> ([a], [a])
partitionOn p t = cata (f t p)
  where
    f t p Nil = ([], [])
    f t p (Cons a (pass, fail))
      | t (p a) = (a : pass, fail)
      | otherwise = (pass, a : fail)

findFirstF :: (a -> Bool) -> ListF a (Maybe a) -> Maybe a
findFirstF _ Nil = Nothing
findFirstF p (Cons this continue) =
  if p this
    then Just this
    else continue

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p = cata (findFirstF p)
