-- Time spent: 90 min

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
