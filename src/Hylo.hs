{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Hylo where

import           Control.Comonad.Cofree
import           Control.Monad.Free
import           Data.Functor.Foldable
import           Data.List
import           Data.Numbers.Primes
import qualified Data.Tree              as T
import           Numeric.Natural
import qualified Prelude                as P
import           Protolude

-- | The moves for the towers of hanoi can be represented
--   as a hylo morphism. The anamorphism builds up the tree
--   and the catamorphism tears down the tree into a list.
hyloHanoi :: Int -> [Int]
hyloHanoi = hylo g f
  where
    f :: Int -> Tree_ Int Int
    f 1 = Leaf 1
    f n = Node n (n - 1) (n - 1)
    g :: Tree_ Int [Int] -> [Int]
    g (Leaf n)     = [n]
    g (Node n l r) = l ++ [n] ++ r

-- | The collatz numbers can be generated by applying the
--   iteration rules in
data CollatzTree_ a
  = OneBranch (Int, Int)
              a
  | TwoBranch (Int, Int)
              a
              a
  deriving (Functor)

type CollatzTree = Fix CollatzTree_

hyloCollatz :: Int -> (Int, Int) -> [[(Int, Int)]]
hyloCollatz depth =
  fmap (filter (not . (`elem` [0, 1, 2, 4]) . snd)) .
  take depth . T.levels . hylo g f
  where
    f :: (Int, Int) -> CollatzTree_ (Int, Int)
    f (colt, n)
      | n `mod` 3 == 2 =
        TwoBranch (colt, n) (colt + 1, (n - 1) `div` 3) (colt + 1, 2 * n)
      | otherwise = OneBranch (colt, n) (colt + 1, 2 * n)
    g :: CollatzTree_ (T.Tree (Int, Int)) -> T.Tree (Int, Int)
    g (OneBranch n e)   = T.Node n [e]
    g (TwoBranch n o e) = T.Node n [o, e]
