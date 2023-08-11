module HDT.Agent (
  Agent (..),
  delay,
  broadcast,
  receive,
) where

import Polysemy
import Polysemy.AtomicState
import Polysemy.Async
import Polysemy.State

import Control.Concurrent.STM
import Numeric.Natural

import Optics

newtype Agent msg a = 
  MkAgent (
    Sem '[
      Async
      , AtomicState Natural
      , State (TChan (Natural, msg))
      , Embed IO
    ] a)
  deriving newtype (Functor, Applicative, Monad)

delay :: Agent msg ()
delay = MkAgent $ atomicModify @Natural (+1)
  
broadcast ::
  msg ->
  Agent msg ()
broadcast m = MkAgent $ do
  slot <- atomicGet @Natural
  chan <- get
  embed $ atomically $ writeTChan chan (slot, m)

receive :: Agent msg msg
receive = MkAgent $ do
  chan :: TChan (Natural, msg) <- get
  (^. _2) <$> embed $ atomically $ readTChan chan
