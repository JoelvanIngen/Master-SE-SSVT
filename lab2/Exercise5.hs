-- Time spent: 90 min

{-
We implemented the transitive closure for a relation.
The algorithm takes the current state of the closure (which is the original relation for the first iteration),
and finds new transitive links. It appends these to the current relation, creating a new relation.
It then compares the length of the new relation to the previous one, which serves as a check whether we have found any
new pairs this iteration. If we have, we start a new iteration, otherwise we return.
-}

module Exercise5 where

import Data.List (nub, sort)

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{-
Iterative closure calculation
Takes current state of closure and original relation
- Finds new transitive links
- Adds new pairs to the current relation, keeping uniqueness
- Checks if length is identical
-- If it is, we are done (no new links found)
-- If it isn't, perform another step (calling self)
-}
closureStep :: Ord a => Rel a -> Rel a -> Rel a
closureStep rel orig =
    let new = rel @@ orig
        next = nub (rel ++ new)
    in
        if length next == length rel
            then rel
            else closureStep next orig

trClos :: Ord a => Rel a -> Rel a
trClos rel = sort $ closureStep rel rel

main :: Ord a => Rel a -> Rel a
main = trClos