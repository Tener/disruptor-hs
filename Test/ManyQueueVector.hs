{-# LANGUAGE BangPatterns #-}

module Test.ManyQueueVector (testManyQueueVector) where

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

testManyQueueVector = do
  print "Test.ManyQueue.testManyQueueVector"
  finished <- newEmptyMVar

  mq <- newMQueue (2 ^ 10)
  
  let elements = [0 .. iTERATIONS]
      
      writer q [] = return ()
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
