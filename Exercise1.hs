-- Time spend: 120 minutes


module Exercise1 where
import Test.QuickCheck


leftSide :: Integer -> Integer
leftSide n = sum [x * x | x <- [1..n]]

rightSide :: Integer -> Integer
rightSide n = n * (n + 1) * (2*n + 1) `div` 6

testIfEqual :: Integer -> Bool
testIfEqual n = let a = abs n in leftSide a == rightSide a


leftSide2 :: Integer -> Integer
leftSide2 n = sum [x * x * x | x <- [1..n]]

rightSide2 :: Integer -> Integer
rightSide2 n = (n * (n + 1) `div` 2)^(2 :: Integer)

testIfEqual2 :: Integer -> Bool
testIfEqual2 n = let a = abs n in leftSide2 a == rightSide2 a


main :: IO ()
main = do
    quickCheck testIfEqual
    quickCheck testIfEqual2
