module Apo where

import           Data.Functor.Foldable
import           Data.List

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn p = apo (f p)
  where
    f :: Eq b => (a -> b) -> [a] -> ListF [a] (Either [[a]] [a])
    f _ [] = Nil
    f q (h:t) =
      let (match, rest) = partition ((==) (q h) . q) t
      in Cons (h : match) (Right rest)
