module HDT.Interpret (effToIO) where

import Polysemy
import Polysemy.Async
import System.Exit

import HDT.Agent

import Control.Concurrent
import Control.Concurrent.STM

effToIO ::
  forall r msg.
  Members '[Async, Embed IO, Final IO] r =>
  TChan msg ->
  InterpreterFor (Agent msg) r
effToIO chan = interpret \case
  Delay ->
    do
      async $ embed $ threadDelay 1000000
      >>= await
      >>= \case
        Just _ -> return mempty
        Nothing -> embed $ die "Delay returned `Nothing`"
  Broadcast m -> do
    async do
      embed $ do
        atomically $ writeTChan chan m
      >>= await
      >>= \case
        Just _ -> return mempty
        Nothing -> embed $ die "Broadcast returned `Nothing`"
  Receive ->
    do
      async $
        embed $ atomically (readTChan chan)
      >>= await
      >>= \case
        Just m' -> return m'
        Nothing -> embed $ die "Receive returned `Nothing`"
