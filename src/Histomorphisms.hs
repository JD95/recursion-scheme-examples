{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Histomorphisms where

import           Debug.Trace

import           Control.Comonad
import           Control.Comonad.Cofree       (Cofree)
import           Control.Comonad.Trans.Cofree (CofreeF (..))
import           Control.Monad
import           Control.Monad.Free
import           Data.Functor.Compose
import           Data.Functor.Foldable
import           Data.List
import           Data.List.NonEmpty
import           Data.Numbers.Primes
import qualified Data.Tree                    as T
import           Numeric.Natural
import qualified Prelude                      as P
import           Protolude                    hiding (trace)

data Item = Item
  { value  :: Double
  , weight :: Double
  } deriving (Show)

instance Eq Item where
  a == b = value a == value b

instance Ord Item where
  a <= b = value a <= value b

knapsack :: Double -> [Item] -> Maybe Item
knapsack maxWeight = histo f
  where
      addValue :: Item -> Item -> Maybe Item
      addValue a b
        | (weight a + weight b) <= maxWeight =
            Just $ Item (value a + value b) (weight a + weight b)
        | weight a <= weight b = Just b
        | weight a > weight b = Just a
        | otherwise = Nothing

      f :: ListF Item (Cofree (ListF Item) (Maybe Item)) -> Maybe Item
      f Nil              = Nothing
      f (Cons item prev) = cata (g item) $ fmap (addValue item =<<) prev

      g :: Item -> CofreeF (ListF Item) (Maybe Item) (Maybe Item) -> Maybe Item
      g item (_ :< Nil) = guard (weight item <= maxWeight) *> Just item
      g item (l :< (Cons v r)) =
        let current = addValue item v
        in fmap maximum . nonEmpty . catMaybes $ [l, current, r]
