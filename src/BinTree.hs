{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module BinTree where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data BinTree v
  = BinNode v (BinTree v) (BinTree v)
  | BinLeaf v
  deriving (Functor)

makeBaseFunctor ''BinTree
