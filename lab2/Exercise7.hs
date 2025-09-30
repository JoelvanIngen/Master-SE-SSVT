module Exercise7 (main) where
import Data.List


{-Is there a difference between the symmetric closure of the transitive closure of a relation R and the
 transitive closure of the symmetric closure of R ?

Answer is Yes they are different blow is an example

Let R = [(1,2)]

then trClos R = [(1,2)]
and  symClos(trClos R) = [(1,2),(2,1)]

symClos R = [(1,2),(2,1)]
and trClos(symClos R) = [(1,1),(1,2),(2,1),(2,2)]

so symClos(trClos R) /= trClos(symClos R)

Time spent 20 mins 

-} 


main:: IO() 
main = do
    let r = [(1,2)]
    print "r = [(1,2)]"
    putStr "symClos(trClos r) = "
    print (symClos(trClos r))
    putStr "trClos(symClos r) = "
    print (trClos(symClos r))

-- Binary relations as a list of pairs
type Rel a = [(a, a)]

-- Inverts a pair
pairInvert :: (a, a) -> (a, a)
pairInvert (x1, x2) = (x2, x1)

-- Recursively iterates through the list, adding each element and its reverse
-- before calling itself on the remainder of the list
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
--symClos (x:xs) = x : pairInvert x : symClos xs
symClos xs = nub (sort (concatMap (\p -> [p, pairInvert p]) xs))


infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

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
