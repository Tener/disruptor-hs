module Test.ManyQueueStream where

-- Part of the code by bas_van_dijk ( http://www.reddit.com/r/haskell/comments/mhfn1/can_haskell_do_100k_tps_with_1ms_latency/c313rlo )

import Control.Monad
import Control.Concurrent
import Conf

data MQueue a = Cons {-# UNPACK #-} !(MVar a) (MQueue a)

newMQueue :: Int -> IO (MQueue a)
newMQueue 0 = error "Can't create empty MQueue!"
newMQueue size = do 
  lst <- replicateM size newEmptyMVar
  let fromList [] = fromList lst
      fromList (mv:mvs) = Cons mv (fromList mvs) 
  -- return $ (listToStream lst) -- fromList lst
  return $ (fromList lst)

listToStream :: [MVar a] -> MQueue a
listToStream all@(x:xs) = let self = go xs self in self
    where
      go [x] self = Cons x self
      go (x:xs) self = Cons x (go xs self)

writeMQueue :: MQueue a -> a -> IO (MQueue a)
writeMQueue (Cons mv mvs) x = do
  putMVar mv x
  return mvs
{-# INLINE writeMQueue #-}

readMQueue :: MQueue a -> IO (MQueue a, a)
readMQueue (Cons mv mvs) = do
  x <- takeMVar mv
  return (mvs, x)
{-# INLINE readMQueue #-}

testMQueue'1P1C = do
  print "Test.MQueue.testMQueue'1P1C"
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
