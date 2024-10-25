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
  (a -> [a]) ->
  (a -> f a -> f a) ->
  (f a -> Maybe (a, f a)) ->
  (Set a, f a) ->
  ListF a (Set a, f a)
nonCyclicPathsFrom neighbors schedule next (seen, nodes) =
  case next nodes of
    Nothing -> Nil
    Just (this, rest) ->
      Cons this $
        let prune = filter (`Set.notMember` seen)
            rest' = foldr schedule rest (prune $ neighbors this)
            seen' = Set.insert this seen
         in (seen', rest')

findFirst :: (a -> Bool) -> ListF a (Maybe a) -> Maybe a
findFirst _ Nil = Nothing
findFirst p (Cons this continue) =
  if p this
    then Just this
    else continue

newtype Path a = Path (NonEmpty a)

instance Eq a => Eq (Path a) where
  (Path x) == (Path y) = x == y

instance Ord a => Ord (Path a) where
  (Path x) <= (Path y) = x < y

search ::
  forall f a.
  (Ord a) =>
  (forall x. x -> f x -> f x) ->
  (forall x. f x -> Maybe (x, f x)) ->
  (a -> [a]) ->
  (a -> Bool) ->
  (f (Path a) -> Maybe (Path a))
search schedule next f p =
  hylo (findFirst isTarget) (nonCyclicPathsFrom progress schedule next) . (,) Set.empty
  where
    isTarget (Path xs) = p $ NE.head xs

    progress :: Path a -> [Path a]
    progress (Path (x :| xs)) = [Path (n :| (x : xs)) | n <- f x]

nextFront :: Seq a -> Maybe (a, Seq a)
nextFront Empty = Nothing
nextFront (x :<| xs) = Just (x, xs)

dfs ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
dfs f p = search (Seq.<|) nextFront f p . Seq.singleton . Path . NE.singleton

bfs ::
  forall a.
  (Ord a) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> Maybe (Path a))
bfs f p = search (flip (Seq.|>)) nextFront f p . Seq.singleton . Path . NE.singleton
