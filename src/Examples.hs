{-# LANGUAGE DeriveFunctor, OverloadedStrings, PatternSynonyms #-}
module Examples where

import qualified Prelude as P
import Protolude
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Functor.Foldable

data Tree_ v a = Node v a a
               | Leaf v
                 deriving (Functor)

type Tree v = Fix (Tree_ v)

node v l r = Fix (Node v l r)

pattern NodeF v l r = Fix (Node v l r)

leaf v = Fix (Leaf v)

pattern LeafF v = Fix (Leaf v)

exampleTree :: Tree Int
exampleTree = node 1 (leaf 2) (node 3 (leaf 4) (node 5 (leaf 6) (leaf 7)))

-- | Catamorphisms are simple folds.
--   Here we get the height of the tree
--   by adding up the nodes
cataExample :: Tree Int -> Int
cataExample = cata f
    where f :: Tree_ Int Int -> Int
          f (Leaf n) = 1
          f (Node _ l _) = l + 1

height = cataExample

-- | Paramorphisms are catas, but we get access
--   to original structure of that term along with
--   it's result. Here, we use information about
--   the original to color the values of the nodes.
paraExample :: Tree Int -> [Color Int]
paraExample = para f
    where f :: Tree_ Int (Tree Int, [Color Int]) -> [Color Int]
          f (Leaf v) = [Yellow v]
          f (Node v l (LeafF _, r)) =  snd l <> [Green v] <> r
          f (Node v l r) = snd l <> [Red v] <> snd r

data Color a = Yellow a | Green a | Red a deriving (Show, Eq, Ord)

grouplayers =  sort . paraExample

-- | Zygomorphisms are folds with a helper function.
--   This lets us run the function f on our current
--   value before determining the next value with g.
zygoExample :: Tree Int -> Int
zygoExample = zygo f g
    where f :: Tree_ Int Children -> Children
          f (Leaf n) = Children 1
          f (Node _ (Children m) (Children n)) = Children (m + n)

          g :: Tree_ Int (Children, Int) -> Int
          g (Leaf n) = n
          g (Node n (lKids,l) (rKids,r))
            | lKids > rKids = l
            | lKids < rKids = r
            | otherwise = n

newtype Children = Children Int deriving (Show, Eq, Ord)

-- | Histomorphisms are folds but with previous answers
--   available in the form of a cofree structure.
histoExample :: [Int] -> Int
histoExample = histo f
    where f :: ListF Int (Cofree (ListF Int) Int) -> Int
          f Nil = 0
          f (Cons prev (next :< others)) = prev + next

-- | Anamorphisms are simple unfolds.
--   Here we create an infinite list of
--   numbers from n to infinity.
anaExample :: Int -> [Int]
anaExample = ana f
    where f :: Int -> ListF Int Int
          f n = Cons n (n + 1)

-- | Apomorphisms unfold, either creating a single
--   value or an entire branch. In this case, choosing
--   left will stop the generation.
apoExample :: Int -> [Int]
apoExample = apo f
    where f :: Int -> ListF Int (Either [Int] Int)
          f n | even n = Cons n (Left [n, n])
              | otherwise = Cons n (Right $ n * 3)


-- | Futumorpisms are unfolds which expand out through
--   many branches at once. Imagine a tree exapanding.
futuExample :: Int -> [Int]
futuExample = futu f
    where f :: Int -> ListF Int (Free (ListF Int) Int)
          f n = Nil


