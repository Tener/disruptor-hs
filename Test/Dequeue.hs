{-# LANGUAGE BangPatterns #-}

module Test.Dequeue (testDequeue) where

import Conf

import Data.Dequeue as D
import Control.Concurrent

testDequeue = do
  let elements = [0 .. iTERATIONS]

  print "Data.Dequeue"
  qv <- newMVar (D.fromList elements :: BankersDequeue Int)
  finished <- newEmptyMVar
  
      
  let writer [] = return ()
      writer (x:xs) = do
                !q <- takeMVar qv
                putMVar qv (pushFront q (x :: Int))
                writer xs

      reader !acc 0 = print acc >> putMVar finished ()
      reader !acc n = do
                !q <- takeMVar qv
                let (me, q') = popBack q
                case me of
                  Nothing -> putMVar qv q >> reader acc n
                  Just x -> putMVar qv q' >> reader (acc+x) (n-1)

--  forkIO $ writer elements
  forkIO $ reader 0 iTERATIONS

  takeMVar finished
  return ()
  
