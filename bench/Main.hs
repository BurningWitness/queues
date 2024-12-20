{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Sequence as Seq
import qualified Queue as Queue
import qualified Queue.Ephemeral as Steque

import           Test.Tasty.Bench



queue_populate :: [a] -> Queue.Queue a
queue_populate = foldl' (flip Queue.enqueue) Queue.empty

steque_populate :: [a] -> Steque.EphemeralQueue a
steque_populate = foldl' (flip Steque.enqueue) Steque.empty

seq_populate :: [a] -> Seq.Seq a
seq_populate = foldl' (Seq.|>) Seq.empty



queue_reduce :: Int -> Queue.Queue a -> IO (Queue.Queue a)
queue_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case Queue.dequeue q of
        Nothing      -> fail $ "Empty at " <> show n
        Just (_, q') -> queue_reduce (n - 1) q'

steque_reduce :: Int -> Steque.EphemeralQueue a -> IO (Steque.EphemeralQueue a)
steque_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case Steque.dequeue q of
        Nothing      -> fail $ "Empty at " <> show n
        Just (_, q') -> steque_reduce (n - 1) q'

seq_reduce :: Int -> Seq.Seq a -> IO (Seq.Seq a)
seq_reduce n !q
  | n <= 0    = pure q
  | otherwise =
      case q of
        Seq.Empty    -> fail $ "Empty at " <> show n
        _ Seq.:<| q' -> seq_reduce (n - 1) q'



queue_wobble :: Queue.Queue a -> [a] -> IO (Queue.Queue a)
queue_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = Queue.enqueue x q
      in case Queue.dequeue q' of
           Nothing       -> fail $ "Empty at " <> show n
           Just (_, q'') -> go (n + 1) q'' ys

steque_wobble :: Steque.EphemeralQueue a -> [a] -> IO (Steque.EphemeralQueue a)
steque_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = Steque.enqueue x q
      in case Steque.dequeue q' of
           Nothing       -> fail $ "Empty at " <> show n
           Just (_, q'') -> go (n + 1) q'' ys

seq_wobble :: Seq.Seq a -> [a] -> IO (Seq.Seq a)
seq_wobble = go (0 :: Int)
  where
    go !_ !q      []  = pure q
    go  n  q (x : ys) =
      let q' = q Seq.|> x
      in case q' of
           Seq.Empty     -> fail $ "Empty at " <> show n
           _ Seq.:<| q'' -> go (n + 1) q'' ys



sizes :: [Int]
sizes = [100, 1000, 10000, 100000]

withSizes :: (Int -> [Benchmark]) -> [Benchmark]
withSizes f = fmap (\n -> bgroup (show n) (f n)) sizes



main :: IO ()
main =
  defaultMain
    [ bgroup "insert" $
        withSizes $ \n ->
          let load = [1..n]

          in [ bench "Steque" $ do
                 whnf steque_populate load

             , bench "Queue" $ do
                 whnf queue_populate load

             , bench "Seq" $ do
                 whnf seq_populate load
             ]

    , bgroup "reduce" $
        withSizes $ \n ->
          let load = [1..n]

          in [ bench "Steque" $ do
                 whnfAppIO (steque_reduce n) $ steque_populate load

             , bench "Queue" $ do
                 whnfAppIO (queue_reduce n) $ queue_populate load

             , bench "Seq" $ do
                 whnfAppIO (seq_reduce n) $ seq_populate load
             ]

    , bgroup "wobble" $
        withSizes $ \n ->
          let base = [1..n]
              load = [1..100000]

          in [ bench "Steque" $ do
                 flip whnfAppIO (steque_populate base, load) $
                   uncurry steque_wobble

             , bench "Queue" $ do
                 flip whnfAppIO (queue_populate base, load) $
                   uncurry queue_wobble

             , bench "Seq" $ do
                 flip whnfAppIO (seq_populate base, load) $
                   uncurry seq_wobble
             ]
    ]
