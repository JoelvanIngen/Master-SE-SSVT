-- Time spent: 60 min

{-
TEST GENERATOR 1
The first test generator was created without QuickCheck. Our strategy was:
- Create a random set length between 0 and 50.
- For (length) iterations, create a random number between -100 and 100. Create a list of all these numbers.

The minimum set length of 0 allows for empty sets, and the upper bound of 50 was chosen arbitrarily.
The random value range was chosen arbitrarily to be between -100 and 100, inclusive.


TEST GENERATOR 2
The second test generator was created using QuickCheck functionality, using the functions `vectorOf` (replacing replicateM)
and `choose` (replacing randomRIO). Our strategy remained the same as the first test generator.

We could have used `arbitrary` instead of `choose (-100, 100)`, but we wanted to keep the range identical to the first test generator.


TEST REPORT FOR GENERATOR 2
We test two properties of the set generator:
- The length of the set cannot be higher than 50 (our upper bound)
- No element of the set can be outside the -100 <= x <= 100 bounds

Both of these test cases pass, so we can conclude that the implementation is correct.
-}

module Exercise1 (main) where

import Control.Monad (replicateM)
import System.Random (randomRIO)
import Test.QuickCheck
    ( choose, generate, vectorOf, forAll, quickCheck, Gen, Property )
import SetOrd ( Set(..), list2set )

--- TEST GENERATOR 1
-- Creates a list of random ints between -100 and 100 with length len
randIntList1 :: Int -> IO [Int]
randIntList1 len = replicateM len (randomRIO (-100, 100))

-- Creates a random-length list of size between 0 and 50 and converts to set
randSet1 :: IO (Set Int)
randSet1 = do
    len <- randomRIO (0, 50)
    xs <- randIntList1 len
    return $ list2set xs


--- TEST GENERATOR 2
-- Creates a list of random ints between -100 and 100 with length len
randIntList2 :: Int -> Gen [Int]
randIntList2 len = vectorOf len (choose (-100, 100))

-- Creates a random-length list of size between 0 and 50 and converts to set
randSet2 :: Gen (Set Int)
randSet2 = do
    len <- choose (0, 50)
    xs <- randIntList2 len
    return $ list2set xs


--- TESTS FOR GENERATOR 2
prop_lenBound :: Property
prop_lenBound = forAll randSet2 (\(Set xs) -> length xs <= 50)

prop_valBound :: Property
prop_valBound = forAll randSet2 (\(Set xs) -> all (\x -> x >= -100 && x <= 100) xs)


main :: IO ()
main = do
    -- Print random sample of set1 and set2
    set1 <- randSet1
    print set1

    set2 <- generate randSet2
    print set2

    -- QuickCheck randSet2
    quickCheck prop_lenBound
    quickCheck prop_valBound
