-- Time spent: 240 min

{-
The current mutators as implemented in Mutation.hs are:
anyList
- Strength: High strength; can produce any list, thus testing a huge variety of potential errors
- Weakness: Very broad and not very specific, so doesn't test any subtle changes to the original output

addElements
- Strength: Adds arbitrary numbers to the start and end of the list
- Weakness: Cannot add elements to the middle of the list and does not modify any existing elements

removeElements
- Strength: Removes elements from the list
- Weakness: Cannot remove elements from start or middle and does not modify any existing elements

Types of output not yet covered by these mutators:
- Reverse list: Test pproperties that rely on the order of the elements
- Value modifying: Keeping the length and position of elements, but changing one or more values
- Order modifying: Keeping the length of elements, but changing the order of the elements (can be a single swap)
- Middle insertion: Inserting an item in the middle (or at least not the head or last) of the list
-}

import Test.QuickCheck
import Mutation

-- Reverses the order of the elements in the output list.
-- Strength: Simple, and a strong test of order-dependence
-- Weakness: Becomes a non-mutant if the original list is a palindrome
reverseList :: [Integer] -> Gen [Integer]
reverseList = return . reverse

-- Changes a random element in the list to an arbitrary value.
-- Strength: Very targeted and tests for value-specific bugs
-- Weakness: Does not change length or overall structure and might be too subtle for many properties
changeRandomElement :: [Integer] -> Gen [Integer]
changeRandomElement [] = return [] -- Cannot mutate an empty list
changeRandomElement xs = do
    -- Select random index
    i <- choose (0, length xs - 1)

    -- Generate new arbitrary value for that element
    newVal <- arbitrary

    -- Get prefix and suffix and reconstruct the list
    let (prefix, suffix) = splitAt i xs

    let mutatedList = case suffix of
                      (_:ys) -> prefix ++ [newVal] ++ ys
                      []     -> prefix ++ [newVal] -- Should not happen if i is valid

    return mutatedList

mutators :: [[Integer] -> Gen [Integer]]
mutators = [anyList, removeElements, addElements, reverseList, changeRandomElement]

main :: IO ()
main = do
    let xs = [1,2,3,4,5]
    print xs -- [1,2,3,4,5]
    reversed <- generate (reverseList xs)
    print reversed -- [5,4,3,2,1]
    sample (changeRandomElement xs) -- several versions of xs with 1 number changed
