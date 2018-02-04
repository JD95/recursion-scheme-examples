{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Futu where

import           Control.Comonad.Cofree
import           Control.Monad.Free
import           Data.Functor.Foldable
import           Data.List
import           Data.Numbers.Primes
import qualified Data.Tree              as T
import           Numeric.Natural
import qualified Prelude                as P
import           Protolude

-- | Futumorphisms are useful when the recursion needs
--   to look ahead in the structure. They are unfolds
--   that let us expand many layers at once, instead
--   of single layers like a apomorphisms. Using
--   project, we can make use of future values.
-- | Here, we can implement the init function which
--   returns all be the last element of a list.
futuInit :: [a] -> [a]
futuInit = futu f
  where
    f :: [a] -> ListF a (Free (ListF a) [a])
    f as =
      case project as of
        Nil       -> Nil
        Cons x [] -> Nil
        Cons x s  -> Cons x (pure s)

-- | In this example, we extrapolate down the list and
--   only take the odd elements.
futuExample :: [a] -> [a]
futuExample = futu f
  where
    f :: [a] -> ListF a (Free (ListF a) [a])
    f as =
      case project as of
        Nil -> Nil
        Cons x s ->
          Cons x $ do
            pure $
              case project s of
                Nil      -> s
                Cons _ t -> t

-- | An implementation of a memoized list of fibbonaci numbers
futuFibs :: [Natural]
futuFibs = futu f [0, 1]
  where
    f :: [Natural] -> ListF Natural (Free (ListF Natural) [Natural])
    f as =
      case project as of
        Cons x s ->
          Cons x $
          pure $
          case project s of
            Cons y _ -> s ++ [x + y]
