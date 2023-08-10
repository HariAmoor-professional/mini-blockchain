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
chainLength = error "TODO: implement chainLength"

slotLeader ::
  Int ->
  Slot ->
  NodeId
slotLeader = error "TODO: implement slotLeader"

chainValid ::
  Int ->
  Slot ->
  Chain ->
  Bool
chainValid = error "TODO: implement chainValid"

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
