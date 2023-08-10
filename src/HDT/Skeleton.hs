module HDT.Skeleton (
  PingPongMessage (..),
  ping,
  pong,
  Slot,
  NodeId,
  Block (..),
  Chain (..),
  BftMessage (..),
) where

import Numeric.Natural (Natural)
import Text.Printf (printf)

import HDT.Agent

data PingPongMessage
  = Ping
  | Pong
  deriving (Show)

ping :: Agent PingPongMessage ()
ping = delay >> broadcast Ping >> go
  where
    go = do
      msg <- receive
      case msg of
        Ping -> go
        Pong -> ping

pong :: Agent PingPongMessage ()
pong = do
  msg <- receive
  case msg of
    Ping -> delay >> broadcast Pong >> pong
    Pong -> pong

type Slot = Natural

type NodeId = Natural

data Block = Block
  { slot :: Slot
  , creator :: NodeId
  }

instance Show Block where
  show b = printf "{%d %d}" (slot b) (creator b)

infixl 5 :>

data Chain
  = Genesis
  | Chain :> Block

instance Show Chain where
  showsPrec _ Genesis = showString "Genesis"
  showsPrec d (c :> b) = showParen (d > 10) $ showsPrec 0 c . showString " :> " . showString (show b)

data BftMessage
  = Time Slot
  | NewChain Chain
  deriving (Show)
