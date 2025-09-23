module Exercise9 where
import Test.QuickCheck
import Data.List
-- Time spent: 108 minutes
-- Determine if two lists are permutation
-- This function only works with inputs that do not have duplicate elements

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = all (`elem` y) x && length x == length y

-- Determine if two lists are permutations of each other with no duplicate elements.
-- This version can detect lists containing dupl
--isPermutation x y 
--    |length x /= length y = False
--    |otherwise = all (\element -> count element x == count element y) x
--    where
--        count elemente = length . filter (== elemente)  


generateList :: Gen [Int]
generateList = fmap nub (listOf arbitrary)


--permutation has several core properties: equality of lengths,self-Permutation, 
--same set of elements, symmetry, 
--transitivity and inversion, which are tested next.

--The set of two Permutation has the same length
prop_sameLength :: [Int] -> [Int] -> Bool
prop_sameLength x y = not (isPermutation x y) || (length x == length y)

--A collection is its own Permutation
prop_selfPermutation :: [Int] -> Bool
prop_selfPermutation x = isPermutation x x

--Two sets that are mutually Permutation have the same elements
prop_sameElement :: [Int] -> [Int] -> Bool
prop_sameElement x y = isPermutation x y == (sort x == sort y)

--The two sets are permutations of each other
prop_symmetry :: [Int] -> [Int] -> Bool
prop_symmetry x y = isPermutation x y == isPermutation y x

--The set of permutations is transitive.
prop_transferability :: [Int] -> [Int] -> [Int] -> Bool
prop_transferability x y z = not (isPermutation x y && isPermutation y z) || isPermutation x z

--A permutation where reversing the set results in the same permutation
prop_reverse :: [Int] -> Bool
prop_reverse x = isPermutation x (reverse x)

{--
    Test Report

    Since duplicate values are not allowed in the lists, I wrote a generator that 
    ensures QuickCheck will only generate lists without duplicates.
    On this basis, the implementation of the isPermutation function can be written in a 
    simplified version, which does not check whether the counts of all elements match.
    For example, in this version [1,1,2] and [2,2,1] would pass the test.

    Based on this, I also provided some self-designed test cases, such as checking whether
    boundary values like a single element or an empty list would pass.
    All tests passed.

    Time spent: 108 minutes
--}
main :: IO ()
main = do

    print $ isPermutation [1,2,3] [3,2,1]     -- True
    print $ isPermutation [1] [1,2,3]         -- False
    print $ isPermutation [1,2,3] [4,5,6]     -- False
    print $ isPermutation [1,2,3] [3,1,2]     -- True
    print $ isPermutation [1,2,3] [1,2,3]     -- True
    print $ isPermutation [1] [1]             -- True
    print $ isPermutation [1] []              -- False
    print $ isPermutation "abcd" "dbac"       -- True
    print $ isPermutation "abc" "def"         -- False
    print $ isPermutation "a" "abcd"          -- False
    print $ isPermutation "a" ""              -- False


    quickCheck (forAll generateList prop_selfPermutation)
    quickCheck (forAll generateList prop_sameLength)
    quickCheck (forAll generateList prop_sameElement)
    quickCheck (forAll generateList prop_symmetry)
    quickCheck (forAll generateList prop_transferability)
    quickCheck (forAll generateList prop_reverse)
