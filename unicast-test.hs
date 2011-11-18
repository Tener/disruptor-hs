
module Main where


import Test.BChan
import Test.BChanFast
import Test.BChanVFast
import Test.BTChan
import Test.Chan
import Test.Dequeue
import Test.Queue
import Test.ManyQueue
import Test.ManyQueueStrict
import Test.ManyQueueVector
import Test.IORefQueue

import Conf
import Util


main = do
  print iTERATIONS

--  timed (print ())
  
--  runTest boundedChanTest
--  runTest boundedChanTestFast
--  runTest boundedChanTestVFast
-- 
--  runTest testManyQueue'1P3C 
  runTest testManyQueue'2P2C'singleQueue
--  runTest testManyQueue'1P1C 
--  runTest testIORefQueue'1P1C
--  runTest testIORefQueue'1P1C'Batch

-- 
--  runTest testManyQueueStrict'1P1C 
--  runTest testManyQueueStrict'1P3C
-- 
--  runTest testManyQueueVector'1P1C 
--  runTest testManyQueueVector'1P3C 


  -- TOO SLOW:

--  runTest boundedTChanTest
--  runTest testDequeue

  -- UNBOUNDED MEMORY:

--  runTest testQueue1
--  runTest chanTest

  -- 1P3C

--  runTest testQueue'1P1C
--  runTest testQueue'1P3C
