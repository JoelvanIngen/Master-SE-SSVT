-- Time spent: 120 min

{- Test report:
We included two properties of factorial numbers, which are:
1. Factorials should be positive (non-negative, non-zero) integers (natural numbers).
2. The factorial of n + 1 is (n + 1) * factorial n

Both of these tests are implemented using QuickCheck Properties, and both pass with 100 tests.
We can thus conclude that the factorial function is statistically correct, as proven by the passing tests.

To test, we take numbers between 0 and 20, where we have chosen the upper bound arbitrarily to prevent exploding numbers.
-}

module Exercise1 (main) where
import Test.QuickCheck (Gen, choose, Property, quickCheck)
import Test.QuickCheck.Property (forAll)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n
    | n > 0 = n * factorial (n - 1)
    | otherwise = error "Negative number"

-- Property 1: Factorials should be positive integers
realNumbers :: Property
realNumbers = forAll genSingleInput $ \x -> factorial x > 0

-- Property 2: Factorial of (n + 1) equals (n + 1) * factorial n
factorialAddOne :: Property
factorialAddOne = forAll genSingleInput $ \n -> factorial (n + 1) == (n + 1) * factorial n

-- Tests :(
genSingleInput :: Gen Integer
genSingleInput = choose (0, 20)

main :: IO ()
main = do
    quickCheck realNumbers
    quickCheck factorialAddOne
