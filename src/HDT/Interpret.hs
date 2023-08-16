module HDT.Interpret (effToIO) where

import HDT.Agent
import Polysemy

import Control.Concurrent
import Control.Concurrent.STM

effToIO ::
  forall r msg.
  Member (Embed IO) r =>
  TChan msg ->
  InterpreterFor (Agent msg) r
effToIO chan = interpret \case
  Delay -> embed $ threadDelay 1000000
  Broadcast m -> embed $ atomically $ writeTChan chan m
  Receive -> embed $ atomically (readTChan chan)
