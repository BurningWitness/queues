-- | A queue data structure, as described in
--
--   * Chris Okasaki, /"Simple and efficient purely functional queues and deques"/, Journal of Functional Programming, 5(4):583–592, October 1995.
--
-- This queue supports \(\mathcal{O}(1)\) worst-case push and pop.
module Data.Queue
  ( -- * Queue
    Queue,
    empty,

    -- * Basic interface
    push,
    pop,

    -- * List conversions
    fromList,
    toList,
  )
where

import qualified Data.List as List

-- | A queue.
data Queue a
  = Queue [a] [a] [a]
  deriving stock (Functor)

instance (Show a) => Show (Queue a) where
  show = show . toList

queue :: [a] -> [a] -> [a] -> Queue a
queue xs ys = \case
  [] -> let xs1 = rotate ys [] xs in Queue xs1 [] xs1
  _ : zs -> Queue xs ys zs

-- rotate ys zs xs = xs ++ reverse ys ++ zs
rotate :: [a] -> NonEmptyList a -> [a] -> [a]
rotate (NonEmptyList y ys) zs = \case
  [] -> y : zs
  x : xs -> x : rotate ys (y : zs) xs

-- | An empty queue.
empty :: Queue a
empty =
  Queue [] [] []

-- | \(\mathcal{O}(1)\). Push an element onto a queue.
push :: a -> Queue a -> Queue a
push x (Queue xs ys zs) =
  queue xs (x : ys) zs

-- | \(\mathcal{O}(1)\). Pop an element off of a queue.
pop :: Queue a -> Maybe (a, Queue a)
pop = \case
  Queue [] _ _ -> Nothing
  Queue (x : xs) ys zs -> Just (x, queue xs ys zs)

-- | Construct a queue from a finite list.
fromList :: [a] -> Queue a
fromList =
  List.foldl' (flip push) empty

-- | Construct a list from a queue.
toList :: Queue a -> [a]
toList =
  List.unfoldr pop

type NonEmptyList a =
  [a]

pattern NonEmptyList :: a -> [a] -> NonEmptyList a
pattern NonEmptyList x xs = x : xs

{-# COMPLETE NonEmptyList #-}
