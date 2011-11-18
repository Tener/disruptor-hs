{-# LANGUAGE BangPatterns #-}

module Test.ManyQueue where 
-- (testManyQueue'1P1C, testManyQueue'1P3C) where

import Control.Concurrent
import Control.Monad

import Conf

newtype MQueue a = MQueue [MVar a]

newMQueue size = do
  lst <- replicateM size newEmptyMVar
  return (MQueue (cycle lst))

writeMQueue :: (MQueue a) -> a -> IO (MQueue a)
writeMQueue (MQueue (x:xs)) el = do
  putMVar x el
  return (MQueue xs)

readMQueue :: (MQueue a) -> IO (MQueue a, a)
readMQueue (MQueue (x:xs)) = do
  el <- takeMVar x
  return ((MQueue xs), el)

testManyQueue'1P1C = do
  print "Test.ManyQueue.testManyQueue'1P1C"
  finished <- newEmptyMVar

  mq <- newMQueue bufferSize
  
  let elements = [0] ++ [1 .. iTERATIONS]
      
      writer _ 0 = putMVar finished ()
      writer q x = do
                  q' <- writeMQueue q x
                  writer q' (x-1)

      writer' _ [] = putMVar finished ()
      writer' q (x:xs) = do
                  q' <- writeMQueue q x
                  writer' q' xs

      reader _ !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)
  
  --forkIO $ writer mq iTERATIONS
  forkIO $ writer' mq elements
  forkIO $ reader mq 0 iTERATIONS

  takeMVar finished
  takeMVar finished

testManyQueue'1P3C = do
  print "Test.ManyQueue.testManyQueue'1P3C"
  let tCount = 3
  finished <- newEmptyMVar

  mqs <- replicateM tCount (newMQueue bufferSize)
  
  let elements = [0 .. iTERATIONS]
      
      writer _ [] = putMVar finished ()
      writer qs (x:xs) = do
                  qs' <- mapM (\q -> writeMQueue q x) qs
                  writer qs' xs

      reader _ !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)
  
  forkIO $ writer mqs elements
  mapM_ (\ mq -> forkIO $ reader mq 0 iTERATIONS) mqs

  replicateM (tCount+1) (takeMVar finished)

  return ()


testManyQueue'2P2C'singleQueue = do
  print "Test.ManyQueue.testManyQueue'2P2C'singleQueue"
  finished <- newEmptyMVar

  mq <- newMQueue bufferSize

  svar <- newMVar 0
  
  let elements = [0] ++ [1 .. iTERATIONS]
      writer' _ [] = putMVar finished ()
      writer' q (x:xs) = do
                  q' <- writeMQueue q x
                  writer' q' xs
      
      writer arg _ 0 = putMVar finished ()
      writer arg q !x = do
                  let y = arg * arg
                  q' <- writeMQueue q y
                  y `seq` writer arg q' (x-1)

      reader _ !acc 0 = do
        incCounter acc
        print acc
        putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)
  
  --forkIO $ writer' mq elements
      
      incCounter v = do
        v' <- takeMVar svar
        putMVar svar (v+v')

      tCount = 4

      iTERATIONS' = iTERATIONS `div` tCount

  v <- newMVar 1
  v' <- takeMVar v

  replicateM tCount (forkIO $ writer v' mq iTERATIONS')
  replicateM tCount (forkIO $ reader mq 0 iTERATIONS')

  replicateM (tCount*2) (takeMVar finished)

  sval <- takeMVar svar

  print ("acc",sval)

  return ()