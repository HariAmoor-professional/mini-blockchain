module HDT.Agent (
  Agent (..),
  delay,
  broadcast,
  receive,
) where

data Agent msg a

instance Functor (Agent msg)

instance Applicative (Agent msg)

instance Monad (Agent msg)

delay :: Agent msg ()
delay = error "TODO: implement delay"

broadcast ::
  msg ->
  Agent msg ()
broadcast = error "TODO: implement broadcast"

receive :: Agent msg msg
receive = error "TODO: implement receive"
