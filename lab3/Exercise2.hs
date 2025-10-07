-- Time spent: x min
module Exercise2 where
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Control.Monad


countSurvivors :: Integer
                  -> [[Integer] -> Integer -> Bool]
                  -> [[Integer] -> Gen [Integer]]
                  -> (Integer -> [Integer])
                  -> IO Integer
countSurvivors n props muts fut = do
    results <- replicateM (fromIntegral n) $ generate $ do
        -- Check if the mutant survives by using the input properties and mutations.
        mutator <- elements muts
        input <- arbitrary :: Gen Integer
        mutateResult <- mutate' mutator props fut input
        -- Ignore if the mutate returns an empty list.
        case mutateResult of
            [] -> return False
            results -> return (all (== True) results)
    -- Return the amout of mutants that survived.
    return $ fromIntegral (length (filter id results))


main :: IO ()
main = do
    -- 139 survivors
    survivors <- countSurvivors 4000 [prop_tenElements] [anyList] multiplicationTable
    print survivors

    -- survivors <- countSurvivors 4000 [prop_firstElementIsInput] [anyList] multiplicationTable
    -- print survivors

    survivors <- countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] [anyList] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_linear] [anyList] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_moduloIsZero] [anyList] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_tenElements] [removeElements] multiplicationTable
    print survivors

    -- survivors <- countSurvivors 4000 [prop_firstElementIsInput] [removeElements] multiplicationTable
    -- print survivors

    survivors <- countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] [removeElements] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_linear] [removeElements] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_moduloIsZero] [removeElements] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_tenElements] [addElements] multiplicationTable
    print survivors

    -- survivors <- countSurvivors 4000 [prop_firstElementIsInput] [addElements] multiplicationTable
    -- print survivors

    survivors <- countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] [addElements] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_linear] [addElements] multiplicationTable
    print survivors

    survivors <- countSurvivors 4000 [prop_moduloIsZero] [addElements] multiplicationTable
    print survivors
