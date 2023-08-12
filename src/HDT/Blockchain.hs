module HDT.Blockchain (
  runIO,
  chainLength,
  slotLeader,
  chainValid,
  clock,
  node,
  runPure,
) where

import HDT.Agent
import HDT.Skeleton

import Polysemy.AtomicState

import Data.Bool.HT

import Numeric.Natural

runIO ::
  Show msg =>
  [Agent msg ()] ->
  IO ()
runIO = error "TODO: implement runIO"

chainLength :: Chain -> Int
chainLength Genesis = 0
chainLength (l :> _) = chainLength l + 1

slotLeader ::
  Int ->
  Slot ->
  NodeId
slotLeader n slot = slot `mod` fromIntegral n

chainValid ::
  Int ->
  Slot ->
  Chain ->
  Bool
chainValid _ currSlot Genesis = currSlot >= 0
chainValid n currSlot (a :> b) =
  currSlot >= slot b -- Not exceeding time
    & creator b == slotLeader n (slot b) -- Correct slot leader
      && slot b
        > ( case a of
              Genesis -> 0
              _ :> a' -> slot a'
          )
      && chainValid n currSlot a -- Strictly increasing

clock :: Agent BftMessage a
clock =
  delay
    >> MkAgent (atomicGet @Natural)
    >>= broadcast . Time
    >> clock

node ::
  Int ->
  NodeId ->
  Agent BftMessage a
node = currentChain Genesis
  where
    currentChain :: Chain -> Int -> NodeId -> Agent BftMessage a
    currentChain l n creator = do
      slot <- MkAgent (atomicGet @Natural)
      if slotLeader n slot == creator
        then do
          NewChain other <- receive
          let newChain =
                ( chainValid other
                    && chainLength other
                    > chainLength l
                    ?: (other, l)
                )
                  :> Block {..}
          broadcast $ NewChain newChain
          currentChain newChain n creator
        else currentChain l n creator

runPure ::
  [Agent msg ()] ->
  [(Natural, msg)]
runPure = error "TODO: implement runPure"
