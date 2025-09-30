
-- Time spend: x min



import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    xs <- arbitrary
    return (list2set xs)


setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) _ = emptySet
setIntersection _ (Set []) = emptySet
setIntersection (Set (x:xs)) set2
    | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
    | otherwise    = setIntersection (Set xs) set2



setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []) set2 = set2
setUnion set1 (Set []) = set1
setUnion (Set (x:xs)) set2 = insertSet x (setUnion (Set xs) set2)


setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) _ = emptySet
setDifference set1 (Set []) = set1
setDifference set1 (Set (y:ys)) = setDifference (deleteSet y set1) (Set ys)


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
prop_IntersectionEmpty :: Set Int -> Bool
prop_IntersectionEmpty set1 =
    isEmpty $ setIntersection emptySet set1

prop_UnionEmpty :: Set Int -> Bool
prop_UnionEmpty set1 =
    setUnion emptySet set1 == set1

prop_DifferenceEmpty :: Set Int -> Bool
prop_DifferenceEmpty set1 =
    isEmpty $ setDifference emptySet set1


main :: IO ()
main = do
    let set1 = list2set [1,2,3]
    let set2 = list2set [2,3,4]

    print $ setIntersection set1 set2 == list2set([2,3])
    print $ setUnion set1 set2 == list2set([1,2,3,4])
    print $ setDifference set1 set2 == list2set([1])

    quickCheck prop_IntersectionEmpty
    quickCheck prop_IntersectionSym
    quickCheck prop_IntersectionSubset

    quickCheck prop_UnionEmpty
    quickCheck prop_UnionSym
    quickCheck prop_UnionSubset

    quickCheck prop_DifferenceEmpty
    quickCheck prop_DifferenceSym
    quickCheck prop_DifferenceSubset
