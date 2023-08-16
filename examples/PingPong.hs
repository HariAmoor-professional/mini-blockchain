module Main where

import HDT.Tasks
import Polysemy

main :: IO ()
main = runIO [ping :: Sem [Agent PingPongMessage, Embed IO] (), pong]
