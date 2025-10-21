-- Time spend: 180 min

{-
The build IOLTS generator follows the format given in LTS.hs
It consists of:
([allStates], [inputLabels], [outputLabels], [LabeledTransition], beginState)
The amount of states, input- outputlabels and transitions can be changed in the code.

It passes all quickChecks given the properties of exercise 1.
Therefor it can be assumed that the generated IOLTS is valid.


Test coverage:
The tests only use a single type of generator so edge cases may not be covered.
It does however test all properties of exercise 1 meaning it covers the basic IOLTS functionality

We also do not test edge cases of the IOLTS explicity. For instance:
What happens if we give an empty IOLTS
Or what happens if we give a super large IOLTS
What happens if we have unreachable state (this one probabliy doesn't matter but we don't know for sure cause we don't test)
Lastly we don't test for invalid IOLTSes because we assume that the generator creates correct IOLTSes
-}

module Exercise2 where

import Data.List
import LTS
import Test.QuickCheck
import Control.Monad

import Exercise1 (prop_MinimalValidIsCorrect, prop_InvalidTransitionStateMakesInvalid,
                  prop_MissingInitialStateMakesInvalid, prop_InputOutputOverlapMakesInvalid,
                  prop_TauInLabelsMakesInvalid, prop_UnknownLabelInTransitionMakesInvalid)


ltsGen :: Gen IOLTS
ltsGen = do
    -- Create states.
    numStates <- choose (1,10)
    let states = [1 .. numStates]
    startingState <- elements states

    -- Create labels.
    numInputLabels <- choose (1, 10)
    numOutputLabels <- choose (1, 10)
    generatedInputLabels <- replicateM numInputLabels (genLabel "?")
    generatedOutputLabels <- replicateM numOutputLabels (genLabel "!")
    let inputLabels = nub generatedInputLabels
    let outputLabels = nub generatedOutputLabels

    -- Create transitions.
    numTransitions <- choose (1, 20)
    generatedTransitions <- replicateM numTransitions (genTransition states inputLabels outputLabels)
    let transitions = nub generatedTransitions

    return (states, inputLabels, outputLabels, transitions, startingState)


-- Make a label that is either input or output with name that is just a number.
genLabel :: String -> Gen Label
genLabel prefix = do
    labelName <- choose (0 :: Integer, 100)
    return $ prefix ++ show labelName


-- Generate transitions with the format (beginState, label, outputState)
genTransition :: [State] -> [Label] -> [Label] -> Gen LabeledTransition
genTransition states inputLabels outputLabels = do
    s0 <- elements states
    s1 <- elements states
    label <- elements (inputLabels ++ outputLabels ++ [tau])
    return (s0, label, s1)


-- Check if the exercise 1 properties hold
main :: IO ()
main = do
    quickCheck prop_MinimalValidIsCorrect

    quickCheck prop_InvalidTransitionStateMakesInvalid

    quickCheck prop_MissingInitialStateMakesInvalid

    quickCheck prop_InputOutputOverlapMakesInvalid

    quickCheck prop_TauInLabelsMakesInvalid

    quickCheck prop_UnknownLabelInTransitionMakesInvalid
