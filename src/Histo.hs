{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Histo where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF (..))
import qualified Control.Comonad.Trans.Cofree as F
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Data.Functor.Base
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Data.Tree
import Debug.Trace
