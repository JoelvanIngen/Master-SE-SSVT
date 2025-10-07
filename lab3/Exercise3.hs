-- Time spent: x min
module Exercise3 where
import Data.List
import Mutation
import MultiplicationTable
import Exercise2 (countSurvivors, multiplicationTableProps2)


-- https://stackoverflow.com/questions/58372236/how-to-find-all-minimum-elements-in-a-list-of-tuples
findSubsetsWithMinSurvivors :: [(Integer, [Int])] -> [[Int]]
findSubsetsWithMinSurvivors [] = []
findSubsetsWithMinSurvivors xs = map snd $ filter ((== minfst) . fst) xs
    where minfst = minimum (map fst xs)


findMinimumLengthSubsets :: [[Int]] -> [[Int]]
findMinimumLengthSubsets [] = []
findMinimumLengthSubsets xs =
    let minLength = minimum (map length xs)
    in filter (\subset -> length subset == minLength) xs


findMinimalPropertySubset :: [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[Int]]
findMinimalPropertySubset props fut = do
    let indexSubsets = tail $ subsequences [0..length props - 1]
        propSubsets = map (\indices -> map (props !!) indices) indexSubsets

    survivorsPerSubset <- mapM (\propSubset -> countSurvivors 1000 propSubset fut) propSubsets

    let zippedSurvivorsAndIndexes = zip survivorsPerSubset indexSubsets
        subsetsWithMinSurvivors = findSubsetsWithMinSurvivors zippedSurvivorsAndIndexes
        minimumLengthSubsets = findMinimumLengthSubsets subsetsWithMinSurvivors

    return minimumLengthSubsets


main :: IO ()
main = do
    minimalSubset <- findMinimalPropertySubset multiplicationTableProps2 multiplicationTable
    print minimalSubset
