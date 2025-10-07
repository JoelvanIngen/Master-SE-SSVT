-- Time spent: x min
module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Control.Monad


countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Integer
countSurvivors n props fut = do
  results <- replicateM (fromIntegral n) $ generate $ do
    -- Check if the mutant survives by using the input properties and mutations.
    mutator <- elements mutators
    input <- arbitrary :: Gen Integer
    propResults <- mutate' mutator props fut input
    -- Ignore if the mutate returns an empty list.
    case propResults of
        [] -> return False
        results -> return (all (== True) results)
    -- Return the amout of mutants that survived.
    return $ fromIntegral (length (filter id results))


main :: IO ()
main = do
    survivors <- countSurvivors 4000 multiplicationTableProps multiplicationTable
    print survivors
