
-- Time spend: 30 min

{-
Test if empty lists are handeled correctly.
Test if the program doesn't crash if there is no valid triplet in the input domain.
Test if there is only 1 output, because according to the question there exists
    exactly 1 pythagorean triplet for which a + b + c = 1000.
Check what numbers are in the triplet and:
- Test if a < b < c.
- Test if a + b + c = 1000.
- Test if a^2 + b^2 = c^2.
- Test if the output of the program is 31875000.
-}


module Euler9 (euler9, main) where


calculateProduct :: [(Integer, Integer, Integer)] -> Integer
calculateProduct [] = 0
calculateProduct [(x, y, z)] = x * y * z


generateTriplets :: [Integer] -> [(Integer, Integer, Integer)]
generateTriplets xs = [(a, b, c) |
                       a <- xs,
                       b <- xs,
                       a < b,
                       let c = 1000 - a - b,
                       b < c,
                       a^2 + b^2 == c^2]


euler9 :: Integer
euler9 = calculateProduct $ generateTriplets [1..998]


main :: IO ()
main = do
    print euler9
