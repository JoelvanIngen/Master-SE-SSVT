
-- Time spend: 150 min

{-
Test report:

The generator uses a domain between -5 and 5 in order to test both
    positive and negative numbers.
It only uses at most 10 numbers in order to make visual inspection
    easier, and larger list should not change the behaviour.

The generator tests are done by munual inspection or order to quickly
    check if the functions work correctly on smaller sets.
And according to these tests the functions work propperly.

The functions are also tested using quickCheck,
The test that are used are checking if:
    - the functions work propperly if one of the sets is empty.
    - the returned set or the input sets are a subset of the other, dependant on the function.
    - If the function returns the same output based on the order of the input sets.

The emptySet quickCheck tests are using Char Sets while the subset and
symmetry test use Int Sets, So 2 domains were used for testing.

These tests all succeeded and from this we can conclude that the functions work propperly.
-}
module Exercise2 where

module Exercise2 where

import Control.Monad (replicateM)
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- Returns the intersection between two given sets.
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) _ = emptySet
setIntersection _ (Set []) = emptySet
setIntersection (Set (x:xs)) set2
    | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
    | otherwise    = setIntersection (Set xs) set2


-- Returns the union between two given sets.
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []) set2 = set2
setUnion set1 (Set []) = set1
setUnion (Set (x:xs)) set2 = insertSet x (setUnion (Set xs) set2)


-- Returns the difference between set1 with set2.
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) _ = emptySet
setDifference set1 (Set []) = set1
setDifference set1 (Set (y:ys)) = setDifference (deleteSet y set1) (Set ys)


-- Creates a list of random ints between -5 and 5 with length len
randIntList :: Int -> IO [Int]
randIntList len = replicateM len (randomRIO (-5, 5))

-- Creates a random-length list of size between 0 and 10 and converts to set
randSet :: IO (Set Int)
randSet = do
    len <- randomRIO (0, 10)
    xs <- randIntList len
    return $ list2set xs


-- Test symetry
prop_IntersectionSym :: Set Int -> Set Int -> Bool
prop_IntersectionSym set1 set2 =
    setIntersection set1 set2 == setIntersection set2 set1

prop_UnionSym :: Set Int -> Set Int -> Bool
prop_UnionSym set1 set2 =
    setUnion set1 set2 == setUnion set2 set1

prop_DifferenceSym :: Set Int -> Set Int -> Bool
prop_DifferenceSym set1 set2 =
    set1 /= set2 --> setDifference set1 set2 /= setDifference set2 set1


-- Subset tests
prop_IntersectionSubset :: Set Int -> Set Int -> Bool
prop_IntersectionSubset set1 set2 =
    subSet (setIntersection set1 set2) set1 && subSet (setIntersection set1 set2) set2

prop_UnionSubset :: Set Int -> Set Int -> Bool
prop_UnionSubset set1 set2 =
    subSet set1 (setUnion set1 set2) && subSet set2 (setUnion set1 set2)

prop_DifferenceSubset :: Set Int -> Set Int -> Bool
prop_DifferenceSubset set1 set2 =
    subSet (setDifference set1 set2) set1


-- Test empty
prop_IntersectionEmpty :: Set Char -> Bool
prop_IntersectionEmpty set1 =
    isEmpty $ setIntersection emptySet set1

prop_UnionEmpty :: Set Char -> Bool
prop_UnionEmpty set1 =
    setUnion emptySet set1 == set1

prop_DifferenceEmpty :: Set Char -> Bool
prop_DifferenceEmpty set1 =
    isEmpty $ setDifference emptySet set1


-- Needed so the quickCheck can generate Sets
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    xs <- arbitrary
    return (list2set xs)


main :: IO ()
main = do
    set1 <- randSet
    set2 <- randSet

    print "--- Generator tests ---"
    print set1
    print set2

    print $ setIntersection set1 set2
    print $ setUnion set1 set2
    print $ setDifference set1 set2


    print "--- quickCheck tests ---"
    quickCheck prop_IntersectionEmpty
    quickCheck prop_IntersectionSym
    quickCheck prop_IntersectionSubset

    quickCheck prop_UnionEmpty
    quickCheck prop_UnionSym
    quickCheck prop_UnionSubset

    quickCheck prop_DifferenceEmpty
    quickCheck prop_DifferenceSym
    quickCheck prop_DifferenceSubset
