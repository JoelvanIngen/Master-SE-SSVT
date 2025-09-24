-- TODO: Increment timer if you're picking up work on this file
-- Time spent: 120 min

{-

-}

module Exercise4 where

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
