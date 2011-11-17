{-# LANGUAGE BangPatterns #-}

module Test.ManyQueueStrict (testManyQueueStrict'1P1C, testManyQueueStrict'1P3C) where

import Control.Concurrent.MVar.Strict
import Control.Concurrent (forkIO)
import Control.Monad
import Control.DeepSeq

import Conf

newtype MQueue a = MQueue [MVar a]

newMQueue size = do
  lst <- replicateM size newEmptyMVar
  return (MQueue (cycle lst))

writeMQueue :: (NFData a) => (MQueue a) -> a -> IO (MQueue a)
writeMQueue (MQueue (x:xs)) el = do
  putMVar x el
  return (MQueue xs)

readMQueue :: (MQueue a) -> IO (MQueue a, a)
readMQueue (MQueue (x:xs)) = do
  el <- takeMVar x
  return ((MQueue xs), el)

testManyQueueStrict'1P1C = do
  print "Test.ManyQueueStrict.testManyQueueStrict'1P1C"
  finished <- newEmptyMVar

  mq <- newMQueue bufferSize

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

testManyQueueStrict'1P3C = do
  print "Test.ManyQueueStrict.testManyQueueStrict'1P3C"
  let tCount = 3
  finished <- newEmptyMVar

  mqs <- replicateM tCount (newMQueue bufferSize)

  let elements = [0 .. iTERATIONS]

      writer qs [] = return ()
      writer qs (x:xs) = do
                  qs' <- mapM (\q -> writeMQueue q x) qs
                  writer qs' xs

      reader q !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)

  forkIO $ writer mqs elements
  mapM_ (\ mq -> forkIO $ reader mq 0 iTERATIONS) mqs

  replicateM tCount (takeMVar finished)
