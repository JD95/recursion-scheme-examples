{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE PatternSynonyms #-}

module Utilities where

import           Data.Functor.Foldable
import qualified Data.Tree             as T
import           Prelude               ()
import           Protolude

data Tree_ v a
  = Node v
         a
         a
  | Leaf v
  deriving (Functor)

type Tree v = Fix (Tree_ v)

node :: v -> Fix (Tree_ v) -> Fix (Tree_ v) -> Fix (Tree_ v)
node v l r = Fix (Node v l r)

pattern NodeF v l r = Fix (Node v l r)

leaf :: v -> Fix (Tree_ v)
leaf v = Fix (Leaf v)

pattern LeafF v = Fix (Leaf v)

unfixTree :: Tree a -> T.Tree a
unfixTree = cata f
  where
    f :: Tree_ a (T.Tree a) -> T.Tree a
    f (Leaf a)     = T.Node a []
    f (Node a l r) = T.Node a [l, r]

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . T.drawTree . fmap show . unfixTree

