{-# LANGUAGE BangPatterns #-}

module Test.Chan where

import Conf

import Control.Concurrent as Chan

-- Best case: 
--  ghc --make -threaded -rtsopts -O3 -fforce-recomp unicast-test.hs
-- ./unicast-test +RTS -sstderr -k2m -A2m -N1 -qg -qa -qb -qw 
chanTest = do
  print "Control.Concurrent.Chan"
  ch <- newChan 
  let xs = [0 .. iTERATIONS]
  forkIO $ Chan.writeList2Chan ch xs

  finished <- newEmptyMVar

  let go 0 !acc _ = print acc >> putMVar finished ()
      go n !acc (x:xs) = go (n-1) (acc+x) xs

  forkIO $ go iTERATIONS 0 =<< Chan.getChanContents ch

  takeMVar finished
  print "FINISHED"
