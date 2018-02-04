{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Gapo where

import           Data.Functor.Foldable

replicate :: Int -> a -> [a]
replicate n a = gapo (g a) f a
  where
    f :: a -> ListF a (Either Int a)
    f a = Cons a (Left 0)
    g :: a -> Int -> ListF a Int
    g a m
      | m == n = Nil
      | otherwise = Cons a (m + 1)
