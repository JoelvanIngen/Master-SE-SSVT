
-- Time spend:

-- Test length
-- Check if the intersection of two non-interceting sets are empty
-- Check if the resulting set is in the powerset of the intersection

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


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


setLength :: Set a -> Int a
setLength (Set xs) = length xs


-- testLength :: Set a -> Set a -> Bool a
-- testLength set1 set2 = setLength $ setIntercection set1 set2 <= setLength set1
--                     && setLength $ setIntercection set1 set2 <= setLength set2
--                     && setLength <= setUnion set1 set2 




main :: IO ()
main = do
    let set1 = list2set [1,2,3]
    let set2 = list2set [2,3,4]

    print $ setIntersection set1 set2
    print $ setUnion set1 set2
    print $ setDifference set1 set2

    -- testLength set1 set2

    isEmpty setIntersection set1 set2 -> isEmpty set1 || isEmpty set2
    isEmpty setUnion set1 set2 -> isEmpty set1 && isEmpty set2
    isEmpty setDifference set1 set2 -> isEmpty set1


