module Exercise5 where

import Data.List
import LTS
import Control.Exception
import System.IO.Unsafe 

{- TestReport   Time spent 290 mins

Next, analyze what's wrong with impls 1-8.

1. Its correct
2.False. When executing the open command in the closed state, the error output "closed" is returned instead of "opened"
3.False. After unlocking, it stays in the locked state (2) instead of entering the closed state (1)
4.False. The lock and unlock commands are interchanged
5.False. The open command returns "open" instead of "opened"
6.False. The door got stuck after three open/close cycles.
7.False. Improper handling of incorrect key codes in certain states. 
   Does not return to 1 after unlocking, entering the wrong state space.
8.False. The semantics of states 3, 5, and 7 were unclear, and many states lacked handling for certain commands.

-}

--True IOLTS specification for the door system
doorSpec :: IOLTS
doorSpec = createIOLTS 
  [(0, "?close", 1), 
   (1, "!closed", 1), 
   (1, "?open", 0), 
   (0, "!opened", 0), 
   (1, "?lock", 2), 
   (2, "!locked", 2), 
   (2, "?unlock", 1), 
   (1, "!unlocked", 1)]

-- Test if the SUT complies with the IOLTS specification 
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT spec sut = 
  all (testTrace spec sut) (limitTraces spec 8)

-- Generate all input traces up to a given depth
limitTraces :: IOLTS -> Int -> [Trace]
limitTraces (_, inputs, _, transitions, q0) maxDepth =
  nub $ concatMap (tracesFromState q0) [0..maxDepth]
  where
    tracesFromState _ 0 = [[]]
    tracesFromState s n = 
      [input : rest | input <- inputs,
                      (src, lbl, dst) <- transitions,
                      src == s, lbl == input,
                      rest <- tracesFromState dst (n-1)]

-- Test a single trace: Check whether the SUT's behavior for a 
--given input sequence complies with the specification.
-- Start the test from the initial state of the specification 
--and the initial state of the SUT (assuming it is q0).
testTrace :: IOLTS -> (State -> Label -> (State, Label)) -> Trace -> Bool
testTrace spec sut trace = testTraceStep spec sut [q0] 0 trace
  where (_, _, _, _, q0) = spec

-- Test each input action of the trace step by step. 
-- Return True if all steps in the trace comply with the specification.
--  False if any violations of the specification are found.
testTraceStep :: IOLTS -> (State -> Label -> (State, Label)) -> [State] -> State -> Trace -> Bool
testTraceStep _ _ _ _ [] = True
testTraceStep spec@(_, _, outputs, transitions, _) sut specStates sutState (action:rest) =
  case safeExecuteSUT sut sutState action of
    Nothing -> False -- SUT crashes， Test fails
    Just (newSutState, sutOutput) ->
      let afterInput = [dst | s <- specStates, (src, lbl, dst) <- transitions,
                              src == s, lbl == action]
          allowedOutputs = [lbl | s <- afterInput, (src, lbl, dst) <- transitions,
                                  src == s, lbl `elem` outputs]
          nextSpecStates = [dst | s <- afterInput, (src, lbl, dst) <- transitions,
                                  src == s, lbl == sutOutput]

      --The SUT's output must be within the allowed outputs of the specification.
      in sutOutput `elem` allowedOutputs && 
         testTraceStep spec sut nextSpecStates newSutState rest

-- Catch possible errors and return the possible types
safeExecuteSUT :: (State -> Label -> (State, Label)) -> State -> Label -> Maybe (State, Label)
safeExecuteSUT sut state action = unsafePerformIO $ 
  catch (evaluate (sut state action) >>= return . Just)
        (\(_ :: SomeException) -> return Nothing)

-- Test all implementations
main :: IO ()
main = do
  putStrLn ("doorImpl1: " ++ show (testLTSAgainstSUT doorSpec doorImpl1))
  putStrLn ("doorImpl2: " ++ show (testLTSAgainstSUT doorSpec doorImpl2))
  putStrLn ("doorImpl3: " ++ show (testLTSAgainstSUT doorSpec doorImpl3))
  putStrLn ("doorImpl4: " ++ show (testLTSAgainstSUT doorSpec doorImpl4))
  putStrLn ("doorImpl5: " ++ show (testLTSAgainstSUT doorSpec doorImpl5))
  putStrLn ("doorImpl6: " ++ show (testLTSAgainstSUT doorSpec doorImpl6))
  putStrLn ("doorImpl7: " ++ show (testLTSAgainstSUT doorSpec doorImpl7))
  putStrLn ("doorImpl8: " ++ show (testLTSAgainstSUT doorSpec doorImpl8))