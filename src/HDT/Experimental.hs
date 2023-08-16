module HDT.Experimental (effPureEval) where

import HDT.Agent
import Polysemy

import Conduit
import Data.Functor.Identity ()

import Data.Maybe

effPureEval ::
  forall r msg. Member (Embed (ConduitT msg msg Identity)) r => InterpreterFor (Agent msg) r
effPureEval = interpret \case
  Delay -> return mempty -- No-op: since everything is stateful anyway
  Broadcast m -> embed $ yield m
  Receive -> fromJust <$> embed await
