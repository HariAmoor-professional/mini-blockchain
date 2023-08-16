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
import HDT.Experimental
import HDT.Impure
import HDT.Skeleton

import Polysemy

import Control.Concurrent.ParallelIO.Local
import Control.Concurrent.STM

import Data.Bool.HT
import Numeric.Natural

import Conduit

runIO :: forall msg. (Show msg) => [Sem [Agent msg, Embed IO] ()] -> IO ()
runIO agents = do
  masterChan :: TChan msg <- newTChanIO
  let dup = atomically $ dupTChan masterChan
  _ <- withPool (length agents + 1) $ \pool ->
    parallel pool $
      printForever masterChan : (runAgent dup <$> agents)
  return mempty
  where
    printForever :: Show msg => TChan msg -> IO ()
    printForever masterChan =
      atomically (readTChan masterChan)
        >>= print
        >> printForever masterChan

    runAgent c a = c >>= flip runAgent' a
    runAgent' chan agent = runM $ effToIO chan $ do agent

chainLength :: Chain -> Int
chainLength Genesis = 0
chainLength (l :> _) = chainLength l + 1

slotLeader :: Int -> Slot -> NodeId
slotLeader n slot = slot `mod` fromIntegral n

chainValid :: Int -> Slot -> Chain -> Bool
chainValid _ currSlot Genesis = currSlot >= 0
chainValid n currSlot (a :> b) =
  currSlot >= slot b -- Not exceeding time
    && creator b == slotLeader n (slot b) -- Correct slot leader
    && slot b
      > ( case a of
            Genesis -> 0
            _ :> a' -> slot a'
        )
    && chainValid n currSlot a -- Strictly increasing

clock :: forall r a. (Member (Agent BftMessage) r) => Sem r a
clock = initClock 0
  where
    initClock t =
      broadcast (Time t)
        >> delay @BftMessage
        >> initClock (t + 1)

node :: forall r a. (Member (Agent BftMessage) r) => Int -> NodeId -> Sem r a
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
              let newChain = l :> Block {..}
              broadcast $ NewChain newChain
              currentChain newChain slot n creator
            else currentChain l slot n creator

-- Implement the `Conduit` interpreter given in HDT.Experimental
runPure ::
  forall msg.
  [Sem '[Agent msg, Embed (ConduitT msg msg Identity)] ()] ->
  [(Natural, msg)]
runPure agents = undefined
  where
    _conduitAgents = runM . effPureEval <$> agents
