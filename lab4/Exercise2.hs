module Exercise2 where

import Control.Monad (replicateM)
import Test.QuickCheck
import Data.List (nub)

import LTS (State, LabeledTransition, Label, IOLTS, tau)
import Exercise1 (validateLTS, prop_MinimalValidIsCorrect, prop_InvalidTransitionStateMakesInvalid,
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


-- type LabeledTransition = (State, Label, State)
genTransition :: [State] -> [Label] -> [Label] -> Gen LabeledTransition
genTransition states inputLabels outputLabels = do
    s0 <- elements states
    s1 <- elements states
    label <- elements (inputLabels ++ outputLabels ++ [tau])
    return (s0, label, s1)


main :: IO ()
main = do
    quickCheck prop_MinimalValidIsCorrect

    quickCheck prop_InvalidTransitionStateMakesInvalid

    quickCheck prop_MissingInitialStateMakesInvalid

    quickCheck prop_InputOutputOverlapMakesInvalid

    quickCheck prop_TauInLabelsMakesInvalid

    quickCheck prop_UnknownLabelInTransitionMakesInvalid
