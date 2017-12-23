{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Examples where

import           Control.Comonad.Cofree
import           Control.Monad.Free
import           Data.Functor.Foldable
import           Data.List
import           Data.Numbers.Primes
import qualified Data.Tree              as T
import           Numeric.Natural
import qualified Prelude                as P
import           Protolude

data Tree_ v a
  = Node v
         a
         a
  | Leaf v
  deriving (Functor)

type Tree v = Fix (Tree_ v)

node v l r = Fix (Node v l r)

pattern NodeF v l r = Fix (Node v l r)

leaf v = Fix (Leaf v)

pattern LeafF v = Fix (Leaf v)

exampleTree :: Tree Int
exampleTree = node 1 (leaf 2) (node 3 (leaf 4) (node 5 (leaf 6) (leaf 7)))

-- | Catamorphisms are simple folds
--
--   Here we get the height of the tree
--   by adding up the nodes.
cataExample :: Tree Int -> Int
cataExample = cata f
  where
    f :: Tree_ Int Int -> Int
    f (Leaf n)     = 1
    f (Node _ l _) = l + 1

height = cataExample

unfixTree :: Tree a -> T.Tree a
unfixTree = cata f
  where
    f :: Tree_ a (T.Tree a) -> T.Tree a
    f (Leaf a)     = T.Node a []
    f (Node a l r) = T.Node a [l, r]

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . T.drawTree . fmap show . unfixTree

partitionOn :: Eq b => (a -> b) -> (b -> Bool) -> [a] -> ([a], [a])
partitionOn p t = cata (f t p)
  where
    f t p Nil = ([], [])
    f t p (Cons a (pass, fail))
      | t (p a) = (a : pass, fail)
      | otherwise = (pass, a : fail)

-- | Paramorphisms are catas, but we get access
--  to original structure of that term along with
--  it's result. Here, we use information about
--  the original to color the values of the nodes.
paraExample :: Tree Int -> [Color Int]
paraExample = para f
  where
    f :: Tree_ Int (Tree Int, [Color Int]) -> [Color Int]
    f (Leaf v)                = [Yellow v]
    f (Node v l (LeafF _, r)) = snd l <> [Green v] <> r
    f (Node v l r)            = snd l <> [Red v] <> snd r

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

-- | Now we need map so we can subtract from each element
cataMap :: (a -> b) -> [a] -> [b]
cataMap f = cata (g f)
  where
    g :: (a -> b) -> ListF a [b] -> [b]
    g _ Nil        = []
    g f (Cons a b) = (f a) : b

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
      \front -> cataMap (f a) (front ++ as) : b (front ++ [a])

-- | Now just partially apply (flip (-)) to get the
-- function we want.
subtractFromOthers :: Num a => [a] -> [[a]]
subtractFromOthers = applyToOthers (flip (-))

data Color a
  = Yellow a
  | Green a
  | Red a
  deriving (Show, Eq, Ord)

grouplayers = sort . paraExample

-- | Zygomorphisms are folds with a helper function
--   that builds up results in parallel to the main
--   folding. In this case we use the helper function
--   to return either the deepest leaf, or the deepest
--   node in our tree.
zygoExample :: Tree Int -> Int
zygoExample = zygo f g
  where
    f :: Tree_ Int Children -> Children
    f (Leaf n)                           = Children 1
    f (Node _ (Children m) (Children n)) = Children (m + n)
    g :: Tree_ Int (Children, Int) -> Int
    g (Leaf n) = n
    g (Node n (lKids, l) (rKids, r))
      | lKids > rKids = l
      | lKids < rKids = r
      | otherwise = n

newtype Children =
  Children Int
  deriving (Show, Eq, Ord)

-- | Histomorphisms are folds but with previous answers
--   available in the form of a cofree structure.
histoExample :: Tree Int -> Int
histoExample = histo f
  where
    f :: Tree_ Int (Cofree (Tree_ Int) Int) -> Int
    f (Leaf n)                               = n
    f (Node v (l :< branchL) (r :< branchR)) = v

-- | Anamorphisms are simple unfolds.
--   Here we create a tree with each layer
--   having node values +1 of the previous
--   layer base on some starting seed.
anaExample :: Int -> Tree Int
anaExample = ana f
  where
    f :: Int -> Tree_ Int Int
    f n = Node n (n + 1) (n + 1)

-- | An example is branching out a factor tree from a
--   starting number.
factorTree :: Int -> Tree Int
factorTree = ana f
  where
    f :: Int -> Tree_ Int Int
    f n
      | isPrime n = Leaf n
      | n == 1 = Leaf 1
      | otherwise = Node n a (n `div` a)
      where
        a = fromMaybe 1 (find (\x -> n `mod` x == 0) [2 .. n])

-- | Apomorphisms unfold, either creating a single
--   value or an entire branch. In this case, choosing
--   left will stop the generation.
apoExample :: Int -> [Int]
apoExample = apo f
  where
    f :: Int -> ListF Int (Either [Int] Int)
    f n
      | even n = Cons n (Left [n, n])
      | otherwise = Cons n (Right $ n * 3)

anaRepeat :: a -> [a]
anaRepeat = ana f
  where
    f :: a -> ListF a a
    f a = Cons a a

data Term
  = A
  | C Term
      Term
  deriving (Show)

grow :: Term -> Term -> Term
grow c d = C c d

rule :: Term -> Maybe Term
rule A       = Nothing
rule (C A A) = Nothing
rule (C a b) = Just b

-- apoTheorem :: Term -> [Term]
-- apoTheorem = apo f
--   where f :: Term -> ListF Term (Either [Term] Term)
--         f A = Cons A (Right $ grow A A)
--         f (C A A) = Cons (C A A) (Right $ grow (C A A) (C A A))
--         f term = case rule term of
--           Just t -> Cons t (Right $ grow t t)
--           Nothing -> Cons term (Right $ grow t t)
gapoReplicate :: Int -> a -> [a]
gapoReplicate n a = gapo (g a) f a
  where
    f :: a -> ListF a (Either Int a)
    f a = Cons a (Left 0)
    g :: a -> Int -> ListF a Int
    g a m
      | m == n = Nil
      | otherwise = Cons a (m + 1)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn p = apo (f p)
  where
    f :: Eq b => (a -> b) -> [a] -> ListF [a] (Either [[a]] [a])
    f _ [] = Nil
    f p (h:t) =
      let (match, rest) = partition ((==) (p h) . p) t
      in Cons (h : match) (Right rest)

-- | The moves for the towers of hanoi can be represented
--   as a hylo morphism. The anamorphism builds up the tree
--   and the catamorphism tears down the tree into a list.
hyloHanoi :: Int -> [Int]
hyloHanoi = hylo g f
  where
    f :: Int -> Tree_ Int Int
    f 1 = Leaf 1
    f n = Node n (n - 1) (n - 1)
    g :: Tree_ Int [Int] -> [Int]
    g (Leaf n)     = [n]
    g (Node n l r) = l ++ [n] ++ r

-- | The collatz numbers can be generated by applying the
--   iteration rules in
data CollatzTree_ a
  = OneBranch (Int, Int)
              a
  | TwoBranch (Int, Int)
              a
              a
  deriving (Functor)

type CollatzTree = Fix CollatzTree_

hyloCollatz :: Int -> (Int, Int) -> [[(Int, Int)]]
hyloCollatz depth =
  fmap (filter (not . (`elem` [0, 1, 2, 4]) . snd)) .
  take depth . T.levels . hylo g f
  where
    f :: (Int, Int) -> CollatzTree_ (Int, Int)
    f (colt, n)
      | n `mod` 3 == 2 =
        TwoBranch (colt, n) (colt + 1, (n - 1) `div` 3) (colt + 1, 2 * n)
      | otherwise = OneBranch (colt, n) (colt + 1, 2 * n)
    g :: CollatzTree_ (T.Tree (Int, Int)) -> T.Tree (Int, Int)
    g (OneBranch n e)   = T.Node n [e]
    g (TwoBranch n o e) = T.Node n [o, e]

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
