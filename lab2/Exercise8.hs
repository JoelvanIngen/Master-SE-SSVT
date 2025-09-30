
-- Time spend: 120 min

{-
Test report:

I did not find the lecture notes so I did not find the Satement in the Show class.
Therefor I made my own show and read cause I am not sure if it needed specific functions.

The tests where ran in order to ensure the given values are valid.
It checks if both the age and height are in a certain range.
It also checks if the read function reads the same input as the show function is given.

It appears all quickChecks are working, therefor it can be concluded
    that the read and show functions work propperly.
-}
module Exercise8 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- Custom datatype
data AgeHeight = AgeHeight Int Int deriving (Eq)


-- Custom show and read implementation
instance Show AgeHeight where
    show (AgeHeight age height) =
        show age ++ " " ++ show height

instance Read AgeHeight where
    readsPrec _ input =
        case words input of
            [a, h] -> [(AgeHeight (read a) (read h), "")]
            _ -> []


-- quickChecks
prop_AgeInRange :: AgeHeight -> Bool
prop_AgeInRange (AgeHeight age _) = 0 <= age && age <= 100

prop_HeightInRange :: AgeHeight -> Bool
prop_HeightInRange (AgeHeight _ height) = 0 <= height && height <= 250

prop_ShowRead :: AgeHeight -> Bool
prop_ShowRead ah = read (show ah) == ah


instance Arbitrary AgeHeight where
    arbitrary = do
        age <- choose (0, 100)
        height <- choose (0, 250)
        return (AgeHeight age height)


main :: IO ()
main = do
    let jasperAgeHeigth = AgeHeight 22 184
    print jasperAgeHeigth

    quickCheck prop_AgeInRange
    quickCheck prop_HeightInRange
    quickCheck prop_ShowRead
