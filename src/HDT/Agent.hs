module HDT.Agent (
  Agent,
  delay,
  broadcast,
  receive,
  Eff (..),
) where

import Polysemy
import Polysemy.Async

type Agent msg a = Sem '[Eff msg, Async, Embed IO, Final IO] a

data Eff msg :: Effect where
  Delay_ :: Eff msg m ()
  Broadcast_ :: msg -> Eff msg m ()
  Receive_ :: Eff msg m msg

$(makeSem ''Eff)

delay :: forall msg. Agent msg ()
delay = delay_ @msg

broadcast :: msg -> Agent msg ()
broadcast = broadcast_

receive :: Agent msg msg
receive = receive_
