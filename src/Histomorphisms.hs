{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Histomorphisms where

import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.Free
import           Data.Functor.Foldable
import           Data.List
import           Data.Numbers.Primes
import qualified Data.Tree              as T
import           Numeric.Natural
import qualified Prelude                as P
import           Protolude

data Item = Item
  { value  :: Double
  , weight :: Double
  } deriving (Show)

instance Eq Item where
  a == b = value a == value b

instance Ord Item where
  a <= b = value a <= value b

addValue :: Double -> Item -> Item -> Maybe Item
addValue maxWeight a b = do
  guard (weight a + weight b <= maxWeight)
  pure $ Item (value a + value b) (weight a + weight b)

maxCheck maxWeight item =
      guard (weight item <= maxWeight) *> Just item

knapsack :: Double -> [Item] -> Maybe Item
knapsack maxWeight = histo f
  where
    f :: ListF Item (Cofree (ListF Item) (Maybe Item)) -> Maybe Item
    f Nil = Nothing
    f (Cons item (_ :< Nil)) = maxCheck maxWeight item
    f (Cons item (rt :< (Cons _ (lt :< _)))) = do
      let left = addValue maxWeight item =<< lt
      let right = addValue maxWeight item =<< rt
      let self = maxCheck maxWeight item
      liftA2 max left right <|> left <|> right <|> self
