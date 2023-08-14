module HDT.Agent (
  Agent (..),
  delay,
  broadcast,
  receive,
) where

import Polysemy
import Polysemy.AtomicState
import Polysemy.Input
import Polysemy.Output

import Numeric.Natural

newtype Agent msg a
  = MkAgent
      ( Sem
          '[ AtomicState Natural
           , Input msg
           , Output (Natural, msg)
           ]
          a
      )
  deriving newtype (Functor, Applicative, Monad)

delay :: Agent msg ()
delay = MkAgent $ atomicModify @Natural (+ 1)

broadcast ::
  msg ->
  Agent msg ()
broadcast m = MkAgent $ do
  t <- atomicGet @Natural
  output (t, m)

receive :: Agent msg msg
receive = MkAgent input
