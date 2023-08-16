module Main where

import HDT.Tasks
import Polysemy

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let n = if null args then 3 else (read (head args) :: Int)
  runIO $
    (clock :: Sem '[Agent BftMessage, Embed IO] ())
      : [node n nid | nid <- [0 .. (fromIntegral $ n - 1)]]
