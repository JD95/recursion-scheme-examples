module Examples where

import Prelude ()
import Protolude
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Functor.Foldable


-- | Catamorphisms are simple folds.
--   Here were just add up the elements.
cataExample :: [Int] -> Int
cataExample = cata f
    where f :: ListF Int Int -> Int
          f Nil = 0
          f (Cons prev current) = prev + current


-- | Paramorphisms are folds, but we get access
--   to original next chunk of the list and our
--   next value.
paraExample :: [Int] -> Int
paraExample = para f
    where f :: ListF Int ([Int], Int) -> Int
          f Nil = 0
          f (Cons prev (orig, current)) = 5


-- | Zygomorphisms are folds with a helper function.
--   This lets us run the function f on our current
--   value before determining the next value with g.
zygoExample :: [Int] -> Int
zygoExample = zygo f g
    where f :: ListF Int Bool -> Bool
          f Nil = True
          f (Cons n prev) = even n && prev

          g :: ListF Int (Bool, Int) -> Int
          g Nil = 0
          g (Cons prev (b, orig)) = 5

-- | Histomorphisms are folds but with previous answers
--   available in the form of a cofree structure.
histoExample :: [Int] -> Int
histoExample = histo f
    where f :: ListF Int (Cofree (ListF Int) Int) -> Int
          f Nil = 0
          f (Cons prev (next :< others)) = prev + next

-- | Anamorphisms are simple unfolds.
--   Here we create an infinite list of
--   numbers from n to infinity.
anaExample :: Int -> [Int]
anaExample = ana f
    where f :: Int -> ListF Int Int
          f n = Cons n (n + 1)

-- | Apomorphisms unfold, either creating a single
--   value or an entire branch. In this case, choosing
--   left will stop the generation.
apoExample :: Int -> [Int]
apoExample = apo f
    where f :: Int -> ListF Int (Either [Int] Int)
          f n | even n = Cons n (Left [n, n])
              | otherwise = Cons n (Right $ n * 3)


-- | Futumorpisms are unfolds which expand out through
--   many branches at once. Imagine a tree exapanding.
futuExample :: Int -> [Int]
futuExample = futu f
    where f :: Int -> ListF Int (Free (ListF Int) Int)
          f n = Nil


