{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Histo where

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

knapsack ::
     (Num value, Ord value, Num weight, Ord weight)
  => weight
  -> [(value, weight)]
  -> Maybe (value, weight)
knapsack maxWeight = histo f
  where
    addValue (valueA, weightA) (valueB, weightB)
      | (weightA + weightB) <= maxWeight =
        Just $ (valueA + valueB, weightA + weightB)
      | weightA <= weightB = Just (valueB, weightB)
      | otherwise = Nothing

    g item (_ :< Nil) = guard (snd item <= maxWeight) *> Just item
    g item (l :< (Cons v r)) =
      fmap maximum . nonEmpty . catMaybes $ [l, addValue item v, r]

    f Nil              = Nothing
    f (Cons item prev) = cata (g item) $ fmap (addValue item =<<) prev

