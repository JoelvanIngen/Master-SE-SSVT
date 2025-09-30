-- Time spent: 30 min

{-
We defined the symClos function, which takes a list of pairs. It then recursively
iterates through this list. Each step it:
- Takes the head and appends it to the new list
- Takes the head and appends its inverted variant to the new list
- Calls itself on the tail

The definition of the symmetric closure is:
"The symmetric closure of a relation R on a set A is the smallest relation R′ that contains R and is symmetric."
(https://www.geeksforgeeks.org/engineering-mathematics/closure-relations/).
Our implementation fits this definition.
-}

module Exercise3 where

import Data.List

-- Binary relations as a list of pairs
type Rel a = [(a, a)]

-- Inverts a pair
pairInvert :: (a, a) -> (a, a)
pairInvert (x1, x2) = (x2, x1)

-- Recursively iterates through the list, adding each element and its reverse
-- before calling itself on the remainder of the list
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos xs = nub (sort (concatMap (\p -> [p, pairInvert p]) xs))
