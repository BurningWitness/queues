-- | A double-ended queue data structure with \(\mathcal{O}(1)^*\) (amortized) operations, as described in
--
--   * Okasaki, Chris. \"Simple and efficient purely functional queues and deques.\" /Journal of functional programming/ 5.4 (1995): 583-592.
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
module Deque
  ( -- * Deque
    Deque (Empty, Front, Back),

    -- ** Initialization
    empty,

    -- * Basic interface
    enqueue,
    enqueueFront,
    dequeue,
    dequeueBack,

    -- * Queries
    isEmpty,
    length,

    -- * Transformations
    map,
    traverse,
    reverse,

    -- * List conversions
    fromList,
    toList,
  )
where

import Data.Bits (unsafeShiftR)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Traversable qualified as Traversable
import Prelude hiding (drop, foldMap, length, map, reverse, span, take, traverse)

-- | A double-ended queue data structure with \(\mathcal{O}(1)^*\) (amortized) operations.
data Deque a
  = Q
      [a]
      {-# UNPACK #-} !Int
      [a]
      {-# UNPACK #-} !Int
  deriving stock (Functor)

instance (Eq a) => Eq (Deque a) where
  (==) :: Deque a -> Deque a -> Bool
  xs == ys =
    Deque.length xs == Deque.length ys && Deque.toList xs == Deque.toList ys

instance Foldable Deque where
  foldMap :: (Monoid m) => (a -> m) -> Deque a -> m
  foldMap f =
    go
    where
      go = \case
        Empty -> mempty
        Front x xs -> f x <> go xs

  elem :: (Eq a) => a -> Deque a -> Bool
  elem x (Q xs _ ys _) =
    List.elem x xs || List.elem x ys

  length :: Deque a -> Int
  length =
    Deque.length

  null :: Deque a -> Bool
  null =
    isEmpty

  toList :: Deque a -> [a]
  toList =
    Deque.toList

instance Monoid (Deque a) where
  mempty :: Deque a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the smaller argument.
instance Semigroup (Deque a) where
  (<>) :: Deque a -> Deque a -> Deque a
  xs <> ys
    -- Either enqueue xs at the front of ys, or ys onto the back of xs, depending on which one would be fewer enqueues.
    | Deque.length xs < Deque.length ys = prepend xs ys
    | otherwise = append xs ys

instance (Show a) => Show (Deque a) where
  show :: Deque a -> String
  show =
    show . Deque.toList

instance Traversable Deque where
  traverse :: (Applicative f) => (a -> f b) -> Deque a -> f (Deque b)
  traverse =
    Deque.traverse

-- | An empty double-ended queue.
pattern Empty :: Deque a
pattern Empty <-
  (dequeue -> Nothing)

-- | The front of a double-ended queue, and the rest of it.
pattern Front :: a -> Deque a -> Deque a
pattern Front x xs <-
  (dequeue -> Just (x, xs))

-- | The back of a double-ended queue, and the rest of it.
pattern Back :: Deque a -> a -> Deque a
pattern Back xs x <-
  (dequeueBack -> Just (xs, x))

{-# COMPLETE Empty, Front #-}

{-# COMPLETE Empty, Back #-}

-- Deque smart constructor, to use when it is possible the front list is too long.
makeDeque1 :: [a] -> Int -> [a] -> Int -> Deque a
makeDeque1 xs xlen ys ylen
  | xlen > (3 * ylen + 1) = Q (List.take xlen1 xs) xlen1 (ys ++ List.reverse (List.drop xlen1 xs)) (xlen + ylen - xlen1)
  | otherwise = Q xs xlen ys ylen
  where
    xlen1 = (xlen + ylen) `unsafeShiftR` 1

-- Deque smart constructor, to use when it is possible the back list is too long.
makeDeque2 :: [a] -> Int -> [a] -> Int -> Deque a
makeDeque2 xs xlen ys ylen
  | ylen > (3 * xlen + 1) = Q (xs ++ List.reverse (List.drop ylen1 ys)) xlen1 (List.take ylen1 ys) ylen1
  | otherwise = Q xs xlen ys ylen
  where
    xlen1 = (xlen + ylen) `unsafeShiftR` 1
    ylen1 = xlen + ylen - xlen1

-- | An empty double-ended queue.
empty :: Deque a
empty =
  Q [] 0 [] 0

-- | \(\mathcal{O}(1)^*\). Enqueue an element at the back of a double-ended queue.
enqueue :: a -> Deque a -> Deque a
enqueue y (Q xs xlen ys ylen) =
  makeDeque2 xs xlen (y : ys) (ylen + 1)

-- | \(\mathcal{O}(1)^*\). Enqueue an element at the front of a double-ended queue.
enqueueFront :: a -> Deque a -> Deque a
enqueueFront x (Q xs xlen ys ylen) =
  makeDeque1 (x : xs) (xlen + 1) ys ylen

-- | \(\mathcal{O}(1)\) front, \(\mathcal{O}(1)^*\) rest. Dequeue an element from the front of a double-ended queue.
dequeue :: Deque a -> Maybe (a, Deque a)
dequeue = \case
  Q [] _ [] _ -> Nothing
  Q [] _ (y : _) _ -> Just (y, empty)
  Q (x : xs) xlen ys ylen -> Just (x, makeDeque2 xs (xlen - 1) ys ylen)

-- | \(\mathcal{O}(1)\) back, \(\mathcal{O}(1)^*\) rest. Dequeue an element from of the back of a double-ended queue.
dequeueBack :: Deque a -> Maybe (Deque a, a)
dequeueBack = \case
  Q [] _ [] _ -> Nothing
  Q (x : _) _ [] _ -> Just (empty, x)
  Q xs xlen (y : ys) ylen -> Just (makeDeque1 xs xlen ys (ylen - 1), y)

-- | \(\mathcal{O}(1)\). Is a double-ended queue empty?
isEmpty :: Deque a -> Bool
isEmpty (Q _ xlen _ ylen) =
  xlen == 0 && ylen == 0

-- | \(\mathcal{O}(1)\). How many elements are in a double-ended queue?
length :: Deque a -> Int
length (Q _ xlen _ ylen) =
  xlen + ylen

-- | \(\mathcal{O}(1)\). Reverse a double-ended queue.
reverse :: Deque a -> Deque a
reverse (Q xs xlen ys ylen) =
  Q ys ylen xs xlen

append :: Deque a -> Deque a -> Deque a
append xs Empty = xs
append xs (Front y ys) = append (enqueue y xs) ys

prepend :: Deque a -> Deque a -> Deque a
prepend Empty ys = ys
prepend (Back xs x) ys = prepend xs (enqueueFront x ys)

-- | \(\mathcal{O}(n)\). Apply a function to every element in a double-ended queue.
map :: (a -> b) -> Deque a -> Deque b
map =
  fmap

-- | \(\mathcal{O}(n)\). Apply a function to every element in a double-ended queue.
traverse :: (Applicative f) => (a -> f b) -> Deque a -> f (Deque b)
traverse f (Q xs xlen ys ylen) =
  (\xs1 ys1 -> Q xs1 xlen ys1 ylen) <$> Traversable.traverse f xs <*> backwards ys
  where
    backwards =
      go
      where
        go = \case
          [] -> pure []
          z : zs -> flip (:) <$> go zs <*> f z

-- | \(\mathcal{O}(n)\). Construct a double-ended queue from a list. The head of the list corresponds to the front of
-- the double-ended queue.
fromList :: [a] -> Deque a
fromList =
  foldr enqueueFront empty

-- | \(\mathcal{O}(n)\). Construct a list from a double-ended queue. The head of the list corresponds to the front of
-- the double-ended queue.
toList :: Deque a -> [a]
toList (Q xs _ ys _) =
  xs ++ List.reverse ys
