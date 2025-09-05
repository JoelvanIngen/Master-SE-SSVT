-- Time spend: 60 minutes

-- Test report:
-- input -> output
-- 1 2 9 -> NoTriangle
-- 1 1 1 -> Equilateral
-- 2 2 3 -> Isosceles
-- 3 4 5 -> Rectangular
-- 2 3 4 -> Other


module Exercise3 where


data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a + b < c || a + c < b || b + c < a = NoTriangle
               | a == b && a == c && b == c = Equilateral
               | a == b || a == c || b == c = Isosceles
               | a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a = Rectangular
               | otherwise = Other


readInts :: IO [Integer]
readInts = fmap (map read.words) getLine


main :: IO ()
main = do
    let userInput = readInts
    [a, b, c] <- userInput
    let triangleType = triangle a b c
    print triangleType
