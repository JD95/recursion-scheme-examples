{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Gapo where

import           Data.Functor.Foldable

-- | gapo :: Corecursive t => (b -> Base t b) -> (a -> Base t (Either b a)) -> a -> t

