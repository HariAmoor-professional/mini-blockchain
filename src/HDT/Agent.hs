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
           , Output msg
           ]
          a
      )
  deriving newtype (Functor, Applicative, Monad)

delay :: Agent msg ()
delay = MkAgent $ atomicModify @Natural (+ 1)

broadcast ::
  msg ->
  Agent msg ()
broadcast = MkAgent . output

receive :: Agent msg msg
receive = MkAgent input
