{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Hylo where

import BinTree
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
hyloHanoi :: Int -> [Int]
hyloHanoi = hylo g f
  where
    f :: Int -> BinTreeF Int Int
    f 1 = BinLeafF 1
    f n = BinNodeF n (n - 1) (n - 1)
    g :: BinTreeF Int [Int] -> [Int]
    g (BinLeafF n) = [n]
    g (BinNodeF n l r) = l ++ [n] ++ r

nonCyclicPathsFrom ::
  forall f a.
  (Ord a) =>
  (Set a -> a -> f a) ->
  (f a -> f a -> f a) ->
  (f a -> Maybe (a, f a)) ->
  (Set a, f a) ->
  ListF a (Set a, f a)
nonCyclicPathsFrom neighbors schedule next (seen, nodes) =
  case next nodes of
    Nothing -> Nil
    Just (this, rest) ->
      Cons this $
        let rest' = schedule (neighbors seen this) rest
            seen' = Set.insert this seen
         in (seen', rest')

traversalPath ::
  forall a.
  Ord a =>
  ([a] -> [a] -> [a]) ->
  (a -> [a]) ->
  (a -> [a])
traversalPath schedule neighbors start =
  ana (nonCyclicPathsFrom f schedule g) (Set.empty, [start])
  where
    f seen this = filter (`Set.notMember` seen) $ neighbors this
    g [] = Nothing
    g (x : xs) = Just (x, xs)

depthFirst :: Ord a => (a -> [a]) -> a -> [a]
depthFirst = traversalPath (<>)

breadthFirst :: Ord a => (a -> [a]) -> a -> [a]
breadthFirst = traversalPath (flip (<>))

findFirst :: (a -> Bool) -> ListF a (Maybe a) -> Maybe a
findFirst _ Nil = Nothing
findFirst p (Cons this continue) =
  if p this
    then Just this
    else continue

newtype Path a = Path {unPath :: NonEmpty a}

instance Eq a => Eq (Path a) where
  (Path x) == (Path y) = x == y

instance Ord a => Ord (Path a) where
  (Path x) <= (Path y) = x < y

search ::
  forall f a.
  (Ord a) =>
  (Set (Path a) -> Path a -> f (Path a)) ->
  (f (Path a) -> f (Path a) -> f (Path a)) ->
  (f (Path a) -> Maybe (Path a, f (Path a))) ->
  (a -> Bool) ->
  (f (Path a) -> Maybe (Path a))
search neighbors schedule next p =
  hylo (findFirst isTarget) (nonCyclicPathsFrom neighbors schedule next) . (,) Set.empty
  where
    isTarget (Path xs) = p $ NE.head xs

nextL :: Seq a -> Maybe (a, Seq a)
nextL Empty = Nothing
nextL (x :<| xs) = Just (x, xs)

progress :: Ord a => (a -> [a]) -> Set (Path a) -> Path a -> Seq (Path a)
progress f seen (Path (x :| xs)) =
  Seq.fromList $
    filter (`Set.notMember` seen) [Path (n :| (x : xs)) | n <- f x]

dfs ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
dfs f p = search (progress f) (<>) nextL p . Seq.singleton . Path . NE.singleton

bfs ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
bfs f p = search (progress f) (flip (<>)) nextL p . Seq.singleton . Path . NE.singleton
