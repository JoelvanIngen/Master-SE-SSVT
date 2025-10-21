-- Time spent: 300 min

module Exercise3 where

import Test.QuickCheck
import LTS
import Data.List
import Exercise2 (ltsGen)

-- Maximum search depth to prevent infinite loops
maxDepth :: Int
maxDepth = 100


-- Finds and returns all traces of an IOLTS up to a maximum depth
straces :: IOLTS -> [Trace]
straces (_, _, _, transitions, initialState) =
    -- Runs entire recursive function, extracts just the traces part from each pair, and removes duplicates
    nub $ map snd $ stracesR maxDepth [(initialState, [])]
    
    where
        -- Recursive function, explores IOLTS to find all traces
        stracesR :: Int -> [(State, Trace)] -> [(State, Trace)]
        stracesR 0 _ = []  -- Terminate search at maximum depth
        stracesR _ [] = []  -- No more pairs to explore
        stracesR depth currentPairs = currentPairs ++ stracesR (depth - 1) nextPairs

            where
                -- Find all next state/trace pairs by exploring one step from currentPairs
                nextPairs =
                    concatMap (\(state, trace) ->
                        [(state', trace ++ [lbl]) |
                        (src, lbl, dst) <- transitions, src == state, state' <- [dst]])
                        currentPairs  -- Apply concatMap to currentPairs


genIOLTSTrace :: Gen Trace
genIOLTSTrace = do
    generatedIOLTS <- ltsGen
    let allTraces = straces generatedIOLTS
    -- Select random element from the list
    elements allTraces
