{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ana where

import           Data.Functor.Foldable
import           Data.List
import           Data.Maybe            (fromMaybe)
import           Data.Numbers.Primes

import           Utilities

-- | Anamorphisms are simple unfolds.
--   Here we create a tree with each layer
--   having node values +1 of the previous
--   layer base on some starting seed.
anaExample :: Int -> Tree Int
anaExample = ana f
  where
    f :: Int -> Tree_ Int Int
    f n = Node n (n + 1) (n + 1)

-- | An example is branching out a factor tree from a
--   starting number.
factorTree :: Int -> Tree Int
factorTree = ana f
  where
    f :: Int -> Tree_ Int Int
    f n
      | isPrime n = Leaf n
      | n == 1 = Leaf 1
      | otherwise = Node n a (n `div` a)
      where
        a = fromMaybe 1 (find (\x -> n `mod` x == 0) [2 .. n])

repeat :: a -> [a]
repeat = ana f
  where
    f :: a -> ListF a a
    f a = Cons a a

replicate :: Natural -> a -> [a]
replicate n a = ana (g a) n
  where
    g :: a -> Natural -> ListF a Natural
    g _ 0   = Nil
    g a' n' = Cons a' (n' - 1)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn p = ana (f p)
  where
    f :: Eq b => (a -> b) -> [a] -> ListF [a] [a]
    f _ [] = Nil
    f q (h:t) =
      let (match, rest) = partition ((==) (q h) . q) t
      in Cons (h : match) rest

init :: [a] -> [a]
init = ana f
  where
    f :: [a] -> ListF a [a]
    f as =
      case project as of
        Nil       -> Nil
        Cons _ [] -> Nil
        Cons x s  -> Cons x s

