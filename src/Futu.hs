{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Futu where

import           Control.Monad.Free
import           Data.Functor.Base     (NonEmptyF (..))
import qualified Data.Functor.Base     as F
import           Data.Functor.Foldable
import           Data.List
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import           Numeric.Natural
import qualified Prelude               as P
import           Protolude             hiding (tail)

-- | futu :: Corecursive t => (a -> Base t (Free (Base t) a)) -> a -> t


-- | In this example, we extrapolate down the list and
--   only take the odd elements.
futuExample :: [a] -> [a]
futuExample = futu f
  where
    f :: [a] -> ListF a (Free (ListF a) [a])
    f as =
      case project as of
        Nil -> Nil
        Cons x s -> Cons x $ pure $
          case project s of
            Nil      -> s
            Cons _ t -> t

-- | An implementation of a memoized list of fibbonaci numbers
futuFibs :: NonEmpty Natural
futuFibs = futu f (0 :| [1])
  where
    f :: NonEmpty Natural -> NonEmptyF Natural (Free (NonEmptyF Natural) (NonEmpty Natural))
    f as =
      let p = project as
      in case F.tail p of
        Just (t :| _)  ->
          NonEmptyF (F.head p) . Just . pure $ t :| [F.head p + t]
        Nothing -> undefined
