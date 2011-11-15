{-# LANGUAGE BangPatterns #-}

module Test.BChan where

import Conf
import Control.Concurrent.BoundedChan as BChan 
import Control.Concurrent as Chan

boundedChanTest = do
  print "Control.Concurrent.BoundedChan"
  ch <- newBoundedChan bufferSize 
  let xs = [0 .. iTERATIONS]
  forkIO $ BChan.writeList2Chan ch xs

  finished <- newEmptyMVar

  let go 0 !acc _ = print acc >> putMVar finished ()
      go n !acc (x:xs) = go (n-1) (acc+x) xs

  forkIO $ go iTERATIONS 0 =<< BChan.getChanContents ch

  takeMVar finished
  print "FINISHED"
