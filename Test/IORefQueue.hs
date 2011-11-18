module Test.IORefQueue where

import Conf

import Control.Concurrent
import Control.Monad
import Data.IORef

import qualified Data.Vector as Vec
import Data.Vector (Vector)
-- import Data.Bits

data IORefQueue a = IORefQueue { 
      readPos :: IORef Int
    , writePos :: IORef Int
    , buffer :: Vector (IORef a) }

newQueue :: Int -> IO (IORefQueue a)
newQueue size = do
  vec <- Vec.replicateM size (newIORef (error "newQueue: field not yet written"))
  r <- newIORef 0
  w <- newIORef 0
  return (IORefQueue { readPos = r, writePos = w, buffer = vec })

writeQueue :: IORefQueue a -> a -> IO ()
writeQueue q val = do
  r <- readIORef (readPos q)
  w <- readIORef (writePos q)
      
  let buf = buffer q
      size = Vec.length buf 
  if (w + 1) `mod` size == r 
   then do -- retry
     yield -- this is really needed, otherwise will lock ourselves out with single threaded execution (-N1 or non-threaded runtime) 
     writeQueue q val
   else do
     let el = buf `Vec.unsafeIndex` w
     -- TODO: safely handle multiple-writer case         
     -- old <- atomicModifyIORef el (\ old -> (val,old))
     -- atomicModifyIORef (writePos q) (\ w' -> if w' /= w then ...)
     
     -- atomicModifyIORef el (\ _ -> (val,()))
     writeIORef el val
     atomicModifyIORef (writePos q) (\ w' -> ((w'+1)`mod`size, ()))
     
     return ()


readQueue :: IORefQueue a -> IO a
readQueue q = do
  r <- readIORef (readPos q)
  w <- readIORef (writePos q)
  
  let buf = buffer q
      size = Vec.length buf
  
  if w == r 
   then yield >> readQueue q
   else do
     let el = buf `Vec.unsafeIndex` r
        
     val <- readIORef el
     atomicModifyIORef (readPos q) (\ p -> ((p+1)`mod`size, ()))
                       
     return val

writeQueueBatch :: IORefQueue a -> [a] -> IO [a]
writeQueueBatch q xs = do
  r <- readIORef (readPos q)
  w <- readIORef (writePos q)
  
  let buf = buffer q
      size = Vec.length buf
      space = (r-w-1) `mod` size
  
      go [] free = do
        atomicModifyIORef (writePos q) (\ v -> ((v+space-free) `mod` size,()))
        return []

      go xs 0 = do
        go [] 0
        return xs
        
      go (x:xs) n = do
        let el = buf `Vec.unsafeIndex` ((r+space-n) `mod` size)
        -- atomicModifyIORef el (\ _ -> (x,()))
        writeIORef el x
        go xs (n-1)

  case space of
    0 -> yield >> return xs
    _ -> go xs space

readQueueBatch :: IORefQueue a -> IO (Vector a)
readQueueBatch q = do
  r <- readIORef (readPos q)
  w <- readIORef (writePos q)

  let buf = buffer q
      size = Vec.length buf
      space = (w-r) `mod` size

  res <- Vec.generateM space (\ i -> readIORef (buf `Vec.unsafeIndex` ((r+i)`mod`size)))

  atomicModifyIORef (readPos q) (\ v -> ((v+space) `mod` size,()))
  
  return res

testIORefQueue'1P1C = do
  print "Test.IORefQueue.testIORefQueue'1P1C"
  finished <- newEmptyMVar

  mq <- newQueue (8 * 1024)
  
  let elements = [0 .. iTERATIONS]
      
      writer [] = putMVar finished ()
      writer (x:xs) = do
        writeQueue mq x
        writer xs

      reader !acc 0 = print acc >> putMVar finished ()
      reader !acc n = do
                  x <- readQueue mq
                  reader (acc+x) (n-1)
  
  forkIO $ writer elements
  forkIO $ reader 0 iTERATIONS

  takeMVar finished
  takeMVar finished

testIORefQueue'1P1C'Batch = do
  print "Test.IORefQueue.testIORefQueue'1P1C'Batch"
  finished <- newEmptyMVar

  mq <- newQueue (2 ^ 15)
  
  let elements = [0 .. iTERATIONS]
      
      writer [] = putMVar finished ()
      writer xs = do
        xs' <- writeQueueBatch mq xs
        writer xs'

      reader !acc 0 = print acc >> putMVar finished ()
      reader !acc n = do
                  x <- readQueue mq
                  reader (acc+x) (n-1)

      readerB !acc 0 = print acc >> putMVar finished ()
      readerB !acc n | n < 0 = do
                              print $ "error: readerB: " ++ show n
                              readerB acc 0
                     | otherwise = do
        vec <- readQueueBatch mq
        readerB (acc + Vec.sum vec) (n - (Vec.length vec))
  
  forkIO $ writer elements
  forkIO $ readerB 0 iTERATIONS

  takeMVar finished
  takeMVar finished

