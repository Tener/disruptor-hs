{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Time

import Conf
import Test.BChan
import Test.BTChan
import Test.Chan
import Test.Dequeue
import Test.Queue
import Test.ManyQueue
import Test.ManyQueueStrict
import Test.ManyQueueVector

import Data.List.Split (splitEvery)

timed act = do
  t0 <- getCurrentTime
  !v <- act
  t1 <- getCurrentTime
  let td = diffUTCTime t1 t0
  let format v = show v -- (show ((fromIntegral v / (10 ** 12)) :: Double))
  print ("Action time: " ++ format td)
  return (v,td)
  

runTest test = do
  (_, t) <- timed test
  print "OpsPerSecond:"

  let format x = unwords . reverse . map reverse . splitEvery 3 . reverse . show $ x

  print $ format (round (fromIntegral iTERATIONS / realToFrac t :: Double) :: Integer)


main = do
  print iTERATIONS

  timed (print ())
  
--  runTest boundedChanTest

  -- TOO SLOW:

--  runTest boundedTChanTest
--  runTest testDequeue

  -- UNBOUNDED MEMORY:

--  runTest testQueue1
--  runTest chanTest

  runTest testManyQueue 
  runTest testManyQueueStrict 
  runTest testManyQueueVector 