-- Time spent: 240 min
{-
Test report:

The countSurvivor function takes properties and mutators as input and checks how many
    mutations are not detected by the properties.

According to the properties the prop_tenElements is the best property because
    it lets the loweset amount of mutants survive, it does mis a few with the anyList mutator
    because those list can be 10 long by chance.

Besides that the removeElement mutator seems to be the hardest mutator to detect.
    three property checks even let all mutants survive.
The other mutators are caught more effectively by the properties. 

But if these properties are combined when all mutators are active they catch all mutants.
    therefor the mutation tests are correct.
-}


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
        -- Check if the mutant survives by using the test properties.
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
    print "----- All mutators and properties -----"
    -- 0 survivors
    survivors <- countSurvivors 4000 multiplicationTableProps mutators multiplicationTable
    print survivors

    print "----- Any List mutator -----"
    -- ~140 survivors
    survivors <- countSurvivors 4000 [prop_tenElements] [anyList] multiplicationTable
    print survivors

    -- ~50 survivors
    survivors <- countSurvivors 4000 [prop_firstElementIsInput] [anyList] multiplicationTable
    print survivors

    -- ~5 survivors
    survivors <- countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] [anyList] multiplicationTable
    print survivors

    -- ~270 survivors
    survivors <- countSurvivors 4000 [prop_linear] [anyList] multiplicationTable
    print survivors

    -- ~320 survivors
    survivors <- countSurvivors 4000 [prop_moduloIsZero] [anyList] multiplicationTable
    print survivors

    print "----- Remove Element mutator -----"
    -- 0 survivors
    survivors <- countSurvivors 4000 [prop_tenElements] [removeElements] multiplicationTable
    print survivors

    -- 4000 survivors
    survivors <- countSurvivors 4000 [prop_firstElementIsInput] [removeElements] multiplicationTable
    print survivors

    -- ~60 survivors
    survivors <- countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] [removeElements] multiplicationTable
    print survivors

    -- 4000 survivors
    survivors <- countSurvivors 4000 [prop_linear] [removeElements] multiplicationTable
    print survivors

    -- 4000 survivors
    survivors <- countSurvivors 4000 [prop_moduloIsZero] [removeElements] multiplicationTable
    print survivors

    print "----- Add Element mutator -----"
    -- 0 survivors
    survivors <- countSurvivors 4000 [prop_tenElements] [addElements] multiplicationTable
    print survivors

    -- ~200 survivors
    survivors <- countSurvivors 4000 [prop_firstElementIsInput] [addElements] multiplicationTable
    print survivors

    -- ~20 survivors
    survivors <- countSurvivors 4000 [prop_sumIsTriangleNumberTimesInput] [addElements] multiplicationTable
    print survivors

    -- 0 survivors
    survivors <- countSurvivors 4000 [prop_linear] [addElements] multiplicationTable
    print survivors

    -- ~200 survivors
    survivors <- countSurvivors 4000 [prop_moduloIsZero] [addElements] multiplicationTable
    print survivors
