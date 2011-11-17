{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}

module Test.Queue (testQueue'1P1C, testQueue'1P3C) where

import Conf

import Data.Queue
import Control.Concurrent
import Control.Concurrent.BoundedChanVFast (BoundedChan) -- BChan 
import Control.Monad(replicateM)

testQueue'1P1C = do
  print "Test.Queue.testQueue'1P1C"
  f <- newFifo :: IO (BoundedChan Int)
  finished <- newEmptyMVar
  let elements = [0 .. iTERATIONS]

      go 0 !acc = print acc >> putMVar finished ()
      go n !acc = do
               mx <- dequeue f
               case mx of
                 Just x -> go (n-1) (acc+x)
                 Nothing -> go n acc

      writer [] = return ()
      writer (x:xs) = enqueue f x >> writer xs

  forkIO $ writer elements -- enqueueBatch f elements
  forkIO $ go iTERATIONS 0

  takeMVar finished

  return ()

testQueue'1P3C = do
  print "Test.Queue.testQueue1'1P3C"
  let tCount = 3
  fifos <- replicateM tCount newFifo :: IO [(BoundedChan Int)]
  finished <- newEmptyMVar
  let elements = [0 .. iTERATIONS]

      go f 0 !acc = print acc >> putMVar finished ()
      go f n !acc = do
               mx <- dequeue f
               case mx of
                 Just x -> go f (n-1) (acc+x)
                 Nothing -> go f n acc

      writer [] = return ()
      writer (x:xs) = mapM_ (\f -> enqueue f x) fifos >> writer xs

  forkIO $ writer elements
  mapM_ (\i -> forkIO $ go (fifos !! i) iTERATIONS 0) [0.. (tCount-1)]

  replicateM tCount (takeMVar finished)

  return ()
