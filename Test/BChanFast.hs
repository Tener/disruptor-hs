{-# LANGUAGE BangPatterns #-}

module Test.BChanFast where

import Conf
import Control.Concurrent.BoundedChanFast as BChan 
import Control.Concurrent as Chan

boundedChanTestFast = do
  print "Control.Concurrent.BoundedChanFast"
  ch <- newBoundedChan bufferSize 
  let xs = [0 .. iTERATIONS]
  forkIO $ BChan.writeList2Chan ch xs

  finished <- newEmptyMVar

  let go 0 !acc _ = print acc >> putMVar finished ()
      go n !acc (x:xs) = go (n-1) (acc+x) xs

  forkIO $ go iTERATIONS 0 =<< BChan.getChanContents ch

  takeMVar finished
  print "FINISHED"
