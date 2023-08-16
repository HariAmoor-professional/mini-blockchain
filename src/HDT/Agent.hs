module HDT.Agent (
  Agent (..),
  delay,
  broadcast,
  receive
) where

import Polysemy

data Agent msg :: Effect where
  Delay :: Agent msg m ()
  Broadcast :: msg -> Agent msg m ()
  Receive :: Agent msg m msg

$(makeSem ''Agent)
