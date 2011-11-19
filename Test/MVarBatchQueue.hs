module Test.MVarBatchQueue where

import Control.Concurrent
import Control.Monad

import Data.IORef

import Conf

type IntT = Int

data Queue a = Queue { readPos :: MVar IntT
                     , writePos :: MVar IntT
                     , buffer :: [IORef a]
                     , capacity :: IntT
                     }

-- snapshot of Queue, i.e. MVars are read 
data Queue' a = Queue' { readPos' :: IntT
                       , writePos' :: IntT
                       , buffer' :: [IORef a]
                       , capacity' :: IntT }

newQueue :: IntT -> IO (Queue a)
newQueue size = do
  lst <- replicateM size (newIORef $ error "newQueue: element not yet written") 
  r <- newMVar 0
  w <- newMVar 0
  return (Queue r w (cycle lst) size)

withQueue :: Queue a -> (Queue' a -> IO (Queue' a, b)) -> IO (Queue a, b)
withQueue q fun = do
  r <- takeMVar (readPos q)
  w <- takeMVar (writePos q)

  let q' = Queue' { readPos' = r
                  , writePos' = w
                  , buffer' = buffer q
                  , capacity' = capacity q }

  (q'', b) <- fun q'
                  
  putMVar (readPos q) (readPos' q'')
  putMVar (writePos q) (writePos' q'')

  yield

  return ((q { buffer = buffer' q'' }), b)

-- a few utility functions. note that we never used (`mod` capacity): read/write markers are always-increasing

{-
freeSpace :: Queue a -> IO IntT
freeSpace q = do
  u <- usedSpace q
  return (capacity q - u)

usedSpace :: Queue a -> IO IntT
usedSpace q = do
  r <- copyMVar (readPos q)
  w <- copyMVar (writePos q)
  return (w-r)
-}

freeSpace' :: Queue' a -> IntT
freeSpace' q = let u = usedSpace' q in capacity' q - u

usedSpace' :: Queue' a -> IntT
usedSpace' q = let r = readPos' q
                   w = writePos' q
               in (w-r)

printE x = error (show x)


-- | write multiple elements in batch mode: read markers once, write multiple elements, write markers once
writeQueueBatch :: Queue a -> [a] -> IO (Queue a, [a])
writeQueueBatch q xs = withQueue q $ \ q' -> do
--  print ("writeQueueBatch", freeSpace' q', usedSpace' q', readPos' q', writePos' q', capacity' q')
  case min 4096 (freeSpace' q') of
    0 -> {- print "writeQueueBatch, retry" >> -} return (q',xs)
    n -> do
      (n', xs', buf') <- writeIORefs n xs (buffer' q')
      -- print n'
      let q'' = q' { writePos' = writePos' q' + n'
                   , buffer' = buf' }
      return (q'', xs')

-- | read multiple elements in batch mode: read markers once, read multiple elements, write markers once
readQueueBatch :: Queue a -> IO (Queue a, (Int,[a]))
readQueueBatch q = withQueue q $ \ q' -> do
--  print ("readQueueBatch", freeSpace' q', usedSpace' q', readPos' q', writePos' q', capacity' q')
  case min 4096 (usedSpace' q') of
    0 -> {- print "readQueueBatch, retry" >> -} return (q',(0,[]))
    n -> do
      (xs, buf') <- readIORefs n (buffer' q')
      let q'' = q' { readPos' = readPos' q' + n
                   , buffer' = buf' }
      return (q'', (n,xs))

-- | write a number of items from a list into a list of iorefs. the item's list may be shorter than supplied number of writes or it may be longer. the iorefs list is supposed to be infinite (cycled)
writeIORefs :: IntT -> [a] -> [IORef a] -> IO (IntT, [a], [IORef a])
writeIORefs n_ xs_ refs_ = let
    go 0 xs' refs' = return (n_ - 0 , xs', refs')
    go n' [] refs' = return (n_ - n', [], refs')
    go n' (x:xs') (r:rs) = writeIORef r x >> go (n'-1) xs' rs
 in go n_ xs_ refs_

-- | read multiple iorefs supplied in a list, return results of reads and shifted list of iorefs. the list is supposed to be infinite (cycled)
readIORefs :: IntT -> [IORef a] -> IO ([a], [IORef a])
readIORefs n refs = let
    go 0 refs' acc = return (acc, refs') -- HACK : reversed. Writer monad anyone? Or maybe just return a Data.Vector.Vector ? 
    go n (r:rs) acc = do
                       x <- readIORef r
                       go (n-1) rs (x:acc)
 in go n refs []

testMVarBatchQueue'1P1C = do
  print "Test.MVarBatchQueue.testMVarBatchQueue'1P1C"
  finished <- newEmptyMVar

  q <- newQueue (2 ^ 15)
  
  let elements = [0 .. iTERATIONS]
      
      writer mq [] = putMVar finished ()
      writer mq xs = do
        (mq',xs') <- writeQueueBatch mq xs
        writer mq' xs'

--      reader !acc 0 = print acc >> putMVar finished ()
--      reader !acc n = do
--                  x <- readQueue mq
--                  reader (acc+x) (n-1)

      readerB _ !acc 0 = print acc >> putMVar finished ()
      readerB mq !acc n | n < 0 = do
                              print $ "error: readerB: " ++ show n
                              readerB mq acc 0
                        | otherwise = do
        (mq',(m,xs)) <- readQueueBatch mq
        readerB mq' (acc + sum xs) (n - m)
  
  forkIO $ writer q elements
  forkIO $ readerB q 0 iTERATIONS

  takeMVar finished
  takeMVar finished



-- readIORefs :: IntT -> [IORef a] -> IO (IntT, [a], [IORef a])
-- readIORefs n xs refs = let
--     go 0 xs' refs' = return (0, xs', refs')
--     go n' [] refs' = return (n', [], refs')
--     go n' (x:xs') (r:rs) = readIORef r x >> go (n'-1) xs' rs
--  in go n xs refs


{-
writeQueue :: a -> Queue a -> IO (Queue a)
writeQueue el q = do
  r <- takeMVar (readPos q)
  w <- takeMVar (writePos q) -- TODO: maybe withMVar will be faster?
  putMVar (writePos q) w
  if r < w then do
             let (ioref : buf) = buffer q
             writeIORef ioref el
             putMVar (readPos q) (r+1)
             return (q { buffer = buf })
  else do
    yield
    writeQueue el q

readQueue :: Queue a -> IO (a, Queue a)
readQueue q = do
  r <- takeMVar (readPos q)
  w <- takeMVar (writePos q) -- TODO: maybe withMVar will be faster?
  putMVar (writePos q) r
  if r < w then do
             let (ioref : buf) = buffer q
                 q' = q { buffer = buf }
             val <- readIORef ioref
             putMVar (writePos q) (r+1)
             return (val,q')
  else do
    yield
    writeQueue el q
  -}           
      
