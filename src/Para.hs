{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Para where

import           Data.Functor.Foldable

-- | Implements a function such that
-- f [a,b,c,d]
-- [[b-a, c-a, d-a], [a-b,c-b,d-b], [a-d,b-d,c-d]]
--
-- First define a function that generates a list of all the
-- other elements for each element.
--
-- others [a,b,c]
-- [[b,c], [a,c], [a,b]]
others :: [a] -> [[a]]
others = ($ []) . para g
  where
    g :: ListF a ([a], ([a] -> [[a]])) -> ([a] -> [[a]])
    g Nil              = const []
    g (Cons a (as, b)) = \front -> (front ++ as) : b (front ++ [a])

-- | Modifing others to take a function and apply with cataMap gives
-- a function which applys each element to a function and then to all
-- the other elements in the list.
--
-- applyToOthers (flip const) reduces to others
applyToOthers :: (a -> a -> a) -> [a] -> [[a]]
applyToOthers f = ($ []) . para (g f)
  where
    g :: (a -> a -> a) -> ListF a ([a], ([a] -> [[a]])) -> ([a] -> [[a]])
    g _ Nil = const []
    g f (Cons a (as, b)) =
      \front -> fmap (f a) (front ++ as) : b (front ++ [a])
