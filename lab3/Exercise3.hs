-- Time spent: 180 min

{-
Test report:
The subsets [prop_tenElements, prop_sumIsTriangleNumberTimesInput] and [prop_tenElements, prop_linear]
    are the minimalProperty subsets.

It first checks what property combination achieves the lowest amount of survivors.
    If the survivor count is not minimal it misses a property that can reduce the survivor count
    Once the set of properties with the minimal survivor count are found the redundant properties are removed by
    only keeping the property sets with the lowest amount of properties.



Both these property subsets achieve a survivor count of 0 meaning they are both minimal property subsets.
-}

module Exercise3 where
import Data.List
import Mutation
import MultiplicationTable
import Exercise2 (countSurvivors)


-- https://stackoverflow.com/questions/58372236/how-to-find-all-minimum-elements-in-a-list-of-tuples'
-- Return the subsets with the lowest amount of survivors.
findSubsetsWithMinSurvivors :: [(Integer, [Int])] -> [[Int]]
findSubsetsWithMinSurvivors [] = []
findSubsetsWithMinSurvivors xs = map snd $ filter ((== minfst) . fst) xs
    where minfst = minimum (map fst xs)


-- Return the subsets with the minimum amount of properties.
findMinimalLengthSubsets :: [[Int]] -> [[Int]]
findMinimalLengthSubsets [] = []
findMinimalLengthSubsets xs =
    let minLength = minimum (map length xs)
    in filter (\subset -> length subset == minLength) xs


-- Find the minimalPropertySubset by checking which combination of properties has the
-- lowest amount of survivors and than removing the sets with redundant properties.
findMinimalPropertySubset :: [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[Int]]
findMinimalPropertySubset props fut = do
    let indexSubsets = tail $ subsequences [0..length props - 1]
        propSubsets = map (\indices -> map (props !!) indices) indexSubsets

    survivorsPerSubset <- mapM (\propSubset -> countSurvivors 4000 propSubset mutators fut) propSubsets

    let zippedSurvivorsAndIndexes = zip survivorsPerSubset indexSubsets
        subsetsWithMinSurvivors = findSubsetsWithMinSurvivors zippedSurvivorsAndIndexes
        minimumLengthSubsets = findMinimalLengthSubsets subsetsWithMinSurvivors

    return minimumLengthSubsets


main :: IO ()
main = do
    minimalSubset <- findMinimalPropertySubset multiplicationTableProps multiplicationTable
    print minimalSubset
