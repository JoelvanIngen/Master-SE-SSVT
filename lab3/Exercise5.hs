module Exercise5 where

import Data.List
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Control.Monad
{-Test Report   Time spent 240 minus
This exercise uses mutation testing to verify the logical relationships between different properties, including:
Equivalence: Whether the results of two properties are exactly the same across all samples;
Subset: Whether "when A is true, B is always true" is true;
Disjunction: Whether both are never true at the same time.
The main implementation strategy is:
Generate multiple sets of sample results by performing pairwise mutation testing on properties.
Based on the returned Boolean pairs (Bool, Bool), calculate and determine whether these properties satisfy equivalence, subset, or other relationships, thereby inferring their logical connections.

The difficulty in this implementation lies in determining whether two properties possess these logical properties.
My initial approach was to compare the results of the two properties by generating inputs multiple times independently, 
but I later realized that this approach wouldn't guarantee the validity of the comparisons, as the property checks must be performed on the same input and under the same mutation conditions to be comparable.
After some reflection, I realized that I could have the program perform the verification of both properties simultaneously in the same mutation cycle, returning the results as a Boolean pair (Bool, Bool).
This way, I could compare the performance of the two properties under the same input and mutation conditions.
-}

--Generate Test Values
testInputs :: Gen [Integer]
testInputs = vectorOf 100 (choose (-100, 100))

{-
Performs multiple mutation tests on the input value and returns verification results for two properties.
This function generates multiple mutated versions using the specified mutator.
For each mutated version, two properties (prop1 and prop2) are verified simultaneously.
Finally, a pair of property verification results for all test samples is returned.
-}
runTwoProps
  :: ([Integer] -> Gen [Integer])           -- mutator function, receives the original list of integers and generates a list of mutated integers
  -> ([Integer] -> Integer -> Bool)         -- prop1 receive property1 verifies whether the mutated list satisfies a specific property
  -> ([Integer] -> Integer -> Bool)         -- prop2 receive property2 verifies whether the mutated list satisfies a specific property
  -> (Integer -> [Integer])                 -- Original Function to be used generates the original list of integers based on the input integer
  -> Integer                                -- The input x
  -> Int                                    -- sampleSize
  -> IO [(Bool, Bool)]
runTwoProps mutator prop1 prop2 originalFun x sampleSize = do
  rows <- replicateM sampleSize (generate $ mutate' mutator [prop1, prop2] originalFun x)
  pure $ map (\[b1, b2] -> (b1, b2)) rows

--Determine whether the two properties are equivalent on this batch of samples. 
--As long as b1 == b2 on each variant, they are considered equivalent.
isEquivalent :: [(Bool,Bool)] -> Bool
isEquivalent = all (uncurry (==))

--Is A included in B? That is, whether A -> B is true for all samples. 
--If True is returned, it means that for each variant in this batch of samples, 
--as long as A is true, B is also true.
isSubset :: [(Bool, Bool)] -> Bool
isSubset = all (\(a,b) -> not a || b)

-- Determine whether disjoint is obtained from the (A,B) result
isDisjunct :: [(Bool,Bool)] -> Bool
isDisjunct = all (\(a,b) -> not (a && b))


main :: IO ()
main = do
    xs <- generate testInputs
    let x = head xs
    

    print ("Test For prop_tenElements and prop_linear: ")
    pairs <- runTwoProps anyList prop_tenElements prop_linear multiplicationTable x 50 
    putStr ("First five results: ")
    print (take 5 pairs)
    putStr ("isEquivalent: ")
    print (isEquivalent pairs)
    putStr ("isSubset: ")
    print (isSubset pairs)
    putStr ("isDisjunct: ")
    print (isDisjunct pairs)


  

