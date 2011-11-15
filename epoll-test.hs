module Main where

import System.Linux.Epoll
import Data.Maybe
import System.Posix.IO (stdInput)

main =  do 
  r <- createRuntime (fromJust . toSize $ 4096)
  withIBuffer r stdInput $ \b -> readBuffer b >>= mapM_ print . take 10 . lines
  shutdownRuntime r
