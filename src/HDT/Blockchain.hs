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
import HDT.Interpret
import HDT.Skeleton (BftMessage (..), Block (..), Chain (..), NodeId, Slot)

import Polysemy
import Polysemy.Async

import Control.Concurrent.ParallelIO.Local
import Control.Concurrent.STM

import Data.Bool.HT
import Numeric.Natural

runIO :: forall msg.  (Show msg) => [Sem [Agent msg, Async, Embed IO, Final IO] ()] -> IO ()
runIO agents = do
  masterChan :: TChan msg <- newTChanIO
  let dup = atomically $ dupTChan masterChan
  _ <- withPool (length agents + 1) $ \pool -> 
    parallel pool 
      $ printForever masterChan : (runAgent dup <$> agents)
  return mempty
  where
    printForever :: Show msg => TChan msg -> IO ()
    printForever masterChan = 
      atomically (readTChan masterChan)
        >>= print
        >> printForever masterChan

    runAgent c a = c >>= flip runAgent' a

    runAgent' chan agent = do
      runFinal
      $ embedToFinal
      $ asyncToIOFinal
      $ effToIO chan
      $ do agent

chainLength :: Chain -> Int
chainLength Genesis = 0
chainLength (l HDT.Skeleton.:> _) = chainLength l + 1

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
chainValid n currSlot (a HDT.Skeleton.:> b) =
  currSlot >= slot b -- Not exceeding time
    && creator b == slotLeader n (slot b) -- Correct slot leader
    && slot b
      > ( case a of
            Genesis -> 0
            _ HDT.Skeleton.:> a' -> slot a'
        )
    && chainValid n currSlot a -- Strictly increasing

clock :: forall r a. (Member (Agent BftMessage) r) => Sem r a
clock = initClock 0
  where
    initClock :: Natural -> Sem r a
    initClock t =
      broadcast (Time t)
        >> delay @BftMessage
        >> initClock (t + 1)

node ::
  forall r a.
  (Member (Agent BftMessage) r) =>
  Int ->
  NodeId ->
  Sem r a
node = currentChain Genesis 0
  where
    currentChain l lastSlot n creator =
      receive >>= \case
        NewChain Genesis -> 
          if chainLength l > 0 
            then currentChain l lastSlot n creator
            else currentChain Genesis lastSlot n creator
        NewChain (c :> b) -> do
          let newChain = chainValid n lastSlot (c :> b) && chainLength (c :> b) > chainLength l ?: (c :> b, l)
          currentChain newChain lastSlot n creator
        Time slot -> 
          if slotLeader n slot == creator && slot > 0
            then do
              let newChain = l :> Block { .. }
              broadcast $ NewChain newChain
              currentChain newChain slot n creator
            else currentChain l slot n creator

runPure ::
  forall msg r.
  (Member (Agent msg) r) =>
  [Sem r ()] ->
  [(Natural, msg)]
runPure _ = undefined
