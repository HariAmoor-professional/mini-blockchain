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
slotLeader n slot = fromIntegral n `mod` slot

chainValid ::
  Int ->
  Slot ->
  Chain ->
  Bool
chainValid _ currSlot Genesis = currSlot == 0
chainValid _ currSlot (Genesis :> b) = currSlot >= slot b && slot b > 0
chainValid _ _ _ = False

clock :: Agent BftMessage a
clock = error "TODO: implement clock"

node ::
  Int ->
  NodeId ->
  Agent BftMessage a
node = error "TODO: implement node"

runPure ::
  [Agent msg ()] ->
  [(Natural, msg)]
runPure = error "TODO: implement runPure"
