{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Zygo where

import           Data.Functor.Foldable

import           Utilities

newtype Children =
  Children Int
  deriving (Show, Eq, Ord)

-- | Zygomorphisms are folds with a helper function
--   that builds up results in parallel to the main
--   folding. In this case we use the helper function
--   to return either the deepest leaf, or the deepest
--   node in our tree.
zygoExample :: Tree Int -> Int
zygoExample = zygo f g
  where
    f :: Tree_ Int Children -> Children
    f (Leaf n)                           = Children 1
    f (Node _ (Children m) (Children n)) = Children (m + n)
    g :: Tree_ Int (Children, Int) -> Int
    g (Leaf n) = n
    g (Node n (lKids, l) (rKids, r))
      | lKids > rKids = l
      | lKids < rKids = r
      | otherwise = n
