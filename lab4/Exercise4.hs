import Data.List
import LTS
import Test.QuickCheck
import Exercise3

{- Test Report   Time spent 260 mins

This test verifies the τ-closures before and after my implementation of the after action:
printAfterForTraces and printAfterForSTraces are driven by traces and straces respectively; 
the judgment criteria are (1) when w ∈ traces, the after iolts w is non-empty; 
(2) the arrival state (set) is consistent with the model's intuitiveness. 
The results show that all the traces pass. For example, coffeeImpl2 arrives at {2} after ["coin"]
On the straces side, the pause trace without delta is consistent with the traces.

In summary, the semantics of after are correctly implemented and have been verified using traces and straces.

-}




-- after :: IOLTS -> Trace -> [State]
-- Compute the set of states reachable from a given trace
after :: IOLTS -> Trace -> [State]
after (_qs, _ins, _outs, lt, q0) w =
  foldl (stepVisible lt) (tauClosure lt [q0]) w

-- Compute all states reachable via zero or more τ transitions 
--until no new states are added via τ transitions
tauClosure :: [LabeledTransition] -> [State] -> [State]
tauClosure lt seeds = fix seeds
  where
    --Merge the current state set with the states 
    --reachable via one τ transition
    step xs = makeSet (xs ++ [ s' | (s,l,s') <- lt, l == tau, s `elem` xs ])
    --Repeated expansion until the state set no longer changes
    fix xs  = let xs' = step xs in if xs' == xs then xs else fix xs'

-- Single-step visible action execution: Apply τ closure 
--before and after executing visible actions
stepVisible :: [LabeledTransition] -> [State] -> Label -> [State]
stepVisible lt ss a =
  let ss0 = tauClosure lt ss  -- τ closure before execution
      ss1 = [ s' | (s,l,s') <- lt, s `elem` ss0, l == a ]
  in tauClosure lt ss1

--Convert IOLTS to LTS, ignoring the distinction between 
--input and output, and using the traces function to process IOLTS.
forgetIO :: IOLTS -> LTS
forgetIO (qs, _ins, _outs, lt, q0) =
  (qs, filter (/= tau) $ makeSet [ l | (_,l,_) <- lt ], lt, q0)

-- Manual test: take the first k traces, 
--feed them to after one by one and print them
printAfterForTraces :: IOLTS -> Int -> IO ()
printAfterForTraces iolts k = do
  let wds = take k (traces (forgetIO iolts))
  mapM_ (\w -> putStrLn $ show w ++ "  =>  " ++ show (after iolts w)) wds


--Take the first k straces, 
--calculate after one by one and display them
printAfterForSTraces :: IOLTS -> Int -> IO ()
printAfterForSTraces iolts k = do
  let wds = take k (straces iolts)
  mapM_ (\w -> putStrLn $ show w ++ "  =>  " ++ show (after iolts w)) wds



main :: IO ()
main = do
  putStrLn "=== Test coffeeImpl2 ==="
  printAfterForSTraces coffeeImpl2 10
  printAfterForTraces coffeeImpl2 10

  putStrLn "=== Test coffeeModel3 ==="
  printAfterForTraces coffeeModel3 10
  printAfterForSTraces coffeeModel3 10

  putStrLn "=== Test coffeeModel4 ==="
  printAfterForTraces coffeeModel4 10
  printAfterForSTraces coffeeModel4 10

  putStrLn "=== Test coffeeImpl5 ==="
  printAfterForTraces coffeeImpl5 10
  printAfterForSTraces coffeeImpl5 10