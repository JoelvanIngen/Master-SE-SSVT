-- Time spent: 360 min

{-
1. Things that can result in invalid IOLTS's are
  - Empty state set (no states exist in the system)
  - Empty transition set (no transitions exist in the system)
  - No initial state in the state set
  - Multiple initial states
  - Transitions that reference states not in the state set
  - Labels in the label sets that don't match existing transitions
  - Label strings that don't follow the '?' and '!' convention
  - Multiple transitions from the same state with the same label
-}

module Exercise1 where

import LTS ( State, LabeledTransition, Label, IOLTS, tau )
import Data.List ( intersect )

{-
Checks validity of all states
Iterates through all transitions, and checks whether
both the start state and end state of each transition is
an element of the states list
-}
allStatesValid :: [State] -> [LabeledTransition] -> Bool
allStatesValid states transitions =
    all (`elem` states) [s | (s, _, _) <- transitions] &&
    all (`elem` states) [s' | (_, _, s') <- transitions] 

{-
Checks validity of all labels
Each label should be either
  - tau
  - Be one of the input labels
  - Be one of the output labels
-}
allLabelsValid :: [Label] -> [Label] -> [LabeledTransition] -> Bool
allLabelsValid inputs outputs transitions =
    let allLabels = inputs ++ outputs ++ [tau]
    in all (`elem` allLabels) [label | (_, label, _) <- transitions]


validateLTS :: IOLTS -> Bool
validateLTS (states, inputs, outputs, transitions, initialState) =
    not (null states) &&  -- States must be non-empty (definition 1)
    initialState `elem` states &&  -- Initial state must be in set of states (definition 1)
    allStatesValid states transitions &&  -- All states mentioned in transition must be in states set (definition 1)
    tau `notElem` (inputs ++ outputs) &&  -- Tau cannot be in label sets (definition 1)
    allLabelsValid inputs outputs transitions &&  -- All labels in transitions must be in inputs, outputs or tau (definition 6)
    null (inputs `intersect` outputs)  -- No input labels can also be an output label (definition 6)


--------------- Properties ----------------
    
-- Minimal valid IOLTS for testing:
minimalValidIOLTS :: IOLTS
minimalValidIOLTS = ([1, 2], ["a"], ["b"], [(1, "a", 2), (2, tau, 1)], 1)

-- Property 1: A minimal, valid IOLTS should return True.
prop_MinimalValidIsCorrect :: Bool
prop_MinimalValidIsCorrect = validateLTS minimalValidIOLTS

-- Property 2: Transition that uses a state not defined in the states list should result False
prop_InvalidTransitionStateMakesInvalid :: Bool
prop_InvalidTransitionStateMakesInvalid =
    let
        -- Transition uses state 99, which is not in [1, 2]
        invalidIOLTS = ([1, 2], ["a"], ["b"], [(1, "a", 99)], 1)
    in
        not (validateLTS invalidIOLTS)

-- Property 3: IOLTS where the initial state is not in the states set should return False
prop_MissingInitialStateMakesInvalid :: Bool
prop_MissingInitialStateMakesInvalid =
    let
        -- Initial state is 5, but only [1, 2] are defined
        invalidIOLTS = ([1, 2], ["a"], ["b"], [(1, "a", 2)], 5)
    in
        not (validateLTS invalidIOLTS)

-- Property 4: IOLTS where an input label is also an output label should return False
prop_InputOutputOverlapMakesInvalid :: Bool
prop_InputOutputOverlapMakesInvalid =
    let
        -- "x" is both input and output
        invalidIOLTS = ([1], ["a", "x"], ["b", "x"], [(1, "a", 1)], 1)
    in
        not (validateLTS invalidIOLTS)

-- Property 5: IOLTS where the tau symbol is defined as an input or output label should return False.
prop_TauInLabelsMakesInvalid :: Bool
prop_TauInLabelsMakesInvalid =
    let
        -- tau is in the inputs list
        invalidIOLTS = ([1], ["a", tau], ["b"], [(1, "a", 1)], 1)
    in
        not (validateLTS invalidIOLTS)

-- Property 6: IOLTS where a transition uses a label that is not an input, output, or tau should return False.
prop_UnknownLabelInTransitionMakesInvalid :: Bool
prop_UnknownLabelInTransitionMakesInvalid =
    let
        -- Transition uses label "test" which is not in ["a"], ["b"], or tau
        invalidIOLTS = ([1, 2], ["a"], ["b"], [(1, "test", 2)], 1)
    in
        not (validateLTS invalidIOLTS)
