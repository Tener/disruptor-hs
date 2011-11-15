{-# LANGUAGE BangPatterns #-}

module Test.BTChan where

import Conf

import Control.Concurrent as Chan
import Control.Concurrent.STM.BTChan as BTChan 
import Control.Concurrent.STM

boundedTChanTest = do
  print "Control.Concurrent.STM.BTChan"
  ch <- atomically $ newBTChan 10 
  let xs = [0 .. iTERATIONS]
  forkIO $ mapM_ (\ x -> atomically $ writeBTChan ch x) xs

--  let writer n | n < 0 = return ()
--               | otherwise = do
--        n' <- atomically $ writerSTM n
--        writer n'
--      
--      writerSTM n = do
--                     b <- tryWriteBTChan ch n
--                     if b then writerSTM (n-1) else return n
-- 
--  forkIO $ writer iTERATIONS

  finished <- newEmptyMVar

  let go 0 !acc = print acc >> putMVar finished ()
      go n !acc = do
        -- x <- atomically $ readBTChan ch
        xs <- readMany
        go (n-length xs) (acc+sum xs)

      readMany = do
        let gostm acc = do
                   m <- tryReadBTChan ch
                   case m of
                     Nothing -> return acc
                     Just x -> gostm (x:acc)
        atomically $ gostm []

  forkIO $ go iTERATIONS 0

  takeMVar finished
  print "FINISHED"
