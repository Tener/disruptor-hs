{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}

module Test.Queue (testQueue1, testQueue2, testQueue3, testQueue4) where

import Conf

import Data.Queue
import Control.Concurrent
import Control.Concurrent.BoundedChan as BChan 

testQueue2 = testQueue1
testQueue3 = testQueue1
testQueue4 = testQueue1

instance NewFifo (BoundedChan a) IO where
    newFifo = newBoundedChan (8 * 1024)

instance Enqueue (BoundedChan a) IO a where
    enqueue = BChan.writeChan
    enqueueBatch = BChan.writeList2Chan

-- instance Dequeue (BoundedChan a) IO a where
--     dequeue ch = fmap Just $ BChan.readChan ch
--     dequeueBatch = BChan.getChanContents

testQueue1 = do
  print "Test.Queue.testQueue1"
  f <- newFifo :: IO (TMVar Int)
  finished <- newEmptyMVar
  let elements = [0 .. iTERATIONS]

      go 0 !acc = print acc >> putMVar finished ()
      go n !acc = do
               mx <- dequeue f
               case mx of
                 Just x -> go (n-1) (acc+x)
                 Nothing -> go n acc

      writer [] = return ()
      writer (x:xs) = enqueue f x

  forkIO $ writer elements -- enqueueBatch f elements
  forkIO $ go iTERATIONS 0

  takeMVar finished