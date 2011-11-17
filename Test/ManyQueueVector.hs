{-# LANGUAGE BangPatterns #-}

module Test.ManyQueueVector (testManyQueueVector'1P1C, testManyQueueVector'1P3C) where

import Control.Concurrent
import Control.Monad
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Data.Bits

import Conf

data MQueue a = MQueue { position :: !Int, 
                         bitmask :: !Int, 
                         vect :: (Vector (MVar a)) }

newMQueue size = do
  let size' = 2 ^ ceiling (logBase 2 size)
      mask = size' - 1
  print ("newMQueue - Vector", size, size')
  vec <- Vec.replicateM size' newEmptyMVar
  return (MQueue 0 mask vec)

writeMQueue :: (MQueue a) -> a -> IO (MQueue a)
writeMQueue mq@(MQueue pos mask vect) el = do
  let x = Vec.unsafeIndex vect pos
      pos' = mask .&. (pos+1)
      mq' = mq { position = pos' }

  putMVar x el
  return mq'

readMQueue :: (MQueue a) -> IO (MQueue a, a)
readMQueue mq@(MQueue pos mask vect) = do
  let x = Vec.unsafeIndex vect pos
      pos' = mask .&. (pos+1)
      mq' = mq { position = pos' }

  el <- takeMVar x
  return (mq', el)

testManyQueueVector'1P1C = do
  print "Test.ManyQueue.testManyQueueVector'1P1C"
  finished <- newEmptyMVar

  mq <- newMQueue (2 ^ 10)
  
  let elements = [0 .. iTERATIONS]
      
      writer q [] = putMVar finished ()
      writer q (x:xs) = do
                  q' <- writeMQueue q x
                  writer q' xs

      reader q !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)
  
  forkIO $ writer mq elements
  forkIO $ reader mq 0 iTERATIONS

  takeMVar finished
  takeMVar finished


testManyQueueVector'1P3C = do
  print "Test.ManyQueue.testManyQueueVector'1P3C"
  let tCount = 3
  finished <- newEmptyMVar

  mqs <- replicateM tCount (newMQueue (2^10))

  let elements = [0 .. iTERATIONS]

      writer qs [] = putMVar finished ()
      writer qs (x:xs) = do
                  qs' <- mapM (\q -> writeMQueue q x) qs
                  writer qs' xs

      reader q !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)

  forkIO $ writer mqs elements
  mapM_ (\ mq -> forkIO $ reader mq 0 iTERATIONS) mqs

  replicateM (tCount+1) (takeMVar finished)
