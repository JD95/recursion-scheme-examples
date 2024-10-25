{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Zygo where

import BinTree
import Data.Functor.Foldable

newtype Children
  = Children Int
  deriving (Show, Eq, Ord)

-- | Zygomorphisms are folds with a helper function
--   that builds up results in parallel to the main
--   folding. In this case we use the helper function
--   to return either the deepest leaf, or the deepest
--   node in our tree.
zygoExample :: BinTree Int -> Int
zygoExample = zygo f g
  where
    f :: BinTreeF Int Children -> Children
    f (BinLeafF n) = Children 1
    f (BinNodeF _ (Children m) (Children n)) = Children (m + n)
    g :: BinTreeF Int (Children, Int) -> Int
    g (BinLeafF n) = n
    g (BinNodeF n (lKids, l) (rKids, r))
      | lKids > rKids = l
      | lKids < rKids = r
      | otherwise = n
