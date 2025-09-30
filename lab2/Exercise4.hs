-- TODO: Increment timer if you're picking up work on this file
-- Time spent: 120 min

{- Test report:

R = {(x, y) | x ≡ y(mod n)}, n > 0.
To be serial, x and y must be in the domain, meaning y(mod n) must also be in the domain.
It could be tested by verifying that in the relation, every x of a domain has a relation to a y where any mod (> 0) of y is x.
To prove that R is serial, 
-}

module Exercise4 where

import Test.QuickCheck

-- List of pairs
type Rel a = [(a,a)]

-- Helper function that determines if the y component of an (x, y) pair is in the domain
pairInDomain :: Eq a => [a] -> (a, a) -> Bool
pairInDomain domain (_, y) = y `elem` domain

-- Filters all pairs where the first value is a specific value
-- `fst` takes the first element from a tuple
-- It then filters the these based on "first item is equal to desired value a"
-- And returns the list of these filtered pairs
pairsByFirstElem :: Eq a => a -> Rel a -> Rel a
pairsByFirstElem a = filter ((== a) . fst)

-- Checks if at least one pair exists for this domain
domainHasPair :: Eq a => [a] -> a -> Rel a -> Bool
domainHasPair domain a rel = any (pairInDomain domain) (pairsByFirstElem a rel)

-- Checks if relation is serial on a given domain
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all (\a -> domainHasPair domain a rel) domain


-- Property 1: in the relation, every element of the domain relates to another element of the domain
prop_allToAny :: Rel a -> Bool
prop_allToAny (r:rs) = False -- TODO: this is basically done in the main code so how to quick test that it does?


-- Property 2:


main :: IO ()
main = do
    quickCheck prop_allToAny