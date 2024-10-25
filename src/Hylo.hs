{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Hylo
  ( -- * Hylomorphisms

    -- | This scheme fuses an Algebra and a Co-Algebra into
    -- a single algorithm without actually fully generating
    -- the intermediate structure.
    hanoi,

    -- ** Searching
    search,
    Path (..),
    dfs,
    bfs,
  )
where

import Ana
import BinTree
import Cata
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- | The moves for the towers of hanoi can be represented
--   as a hylo morphism. The anamorphism builds up the tree
--   and the catamorphism tears down the tree into a list.
hanoi :: Int -> [Int]
hanoi = hylo g f
  where
    f :: Int -> BinTreeF Int Int
    f 1 = BinLeafF 1
    f n = BinNodeF n (n - 1) (n - 1)

    g :: BinTreeF Int [Int] -> [Int]
    g (BinLeafF n) = [n]
    g (BinNodeF n l r) = l ++ [n] ++ r

-- | Search algorithms over some graph
--   Here we can reuse `traversalPathF` and `findFirstF`
--   and fuse the two together, running the search without
--   generating the entire traversal first
search ::
  forall f a.
  (Ord a) =>
  -- | Produce neighbors for the current node
  -- with the context of nodes seen before
  (Set a -> a -> f a) ->
  -- | From the schedule of nodes to visit
  -- pop off the next one
  (f a -> Maybe (a, f a)) ->
  -- | Given a new batch of nodes to visit
  -- add them to the current schedule
  (f a -> f a -> f a) ->
  -- | Predicate for the target node
  (a -> Bool) ->
  (f a -> Maybe a)
search neighbors next schedule isTarget =
  hylo (findFirstF isTarget) (traversalPathF neighbors schedule next) . (,) Set.empty

sequenceSearch ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (Seq a -> Seq a -> Seq a) ->
  (a -> Bool) ->
  (a -> Maybe a)
sequenceSearch neighbors schedule isTarget start =
  search f g schedule isTarget (Seq.singleton start)
  where
    f seen this = Seq.fromList . filter (`Set.notMember` seen) $ neighbors this

    g Empty = Nothing
    g (x :<| xs) = Just (x, xs)

newtype Path a = Path {unPath :: NonEmpty a}

instance Eq a => Eq (Path a) where
  (Path x) == (Path y) = x == y

instance Ord a => Ord (Path a) where
  (Path x) <= (Path y) = x < y

pathSearch ::
  forall a.
  (Ord a) =>
  (Seq (Path a) -> Seq (Path a) -> Seq (Path a)) ->
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
pathSearch schedule f p =
  sequenceSearch (pathToNeighbors f) schedule (p . NE.head . unPath) . Path . NE.singleton

pathToNeighbors :: Ord a => (a -> [a]) -> Path a -> [Path a]
pathToNeighbors f (Path (x :| xs)) = [Path (n :| (x : xs)) | n <- f x]

dfs ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
dfs = pathSearch (<>)

bfs ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
bfs = pathSearch (flip (<>))
