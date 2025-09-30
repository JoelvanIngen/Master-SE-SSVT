
-- Time spent: 240 min

{-Proof:

Definition of a serial relation: A relation R is serial over a field A 
if and only if for every element a in A, there exists some element b in A such that (a, b) ∈ R.

Reflexivity of modular arithmetic: In modular n arithmetic, for any integer x, x ≡ x (mod n). 
This is a fundamental property of modular arithmetic.

Constructive proof: For any element x in a field A, let y = x. 
Then: x ≡ x (mod n) holds (by reflexivity).

Therefore, (x, x) ∈ R.
Since x ∈ A and y = x ∈ A, for every x ∈ A, 
there exists y ∈ A such that (x, y) ∈ R.
Conclusion: Since for every element in a field A, we can find a corresponding 
element (i.e., the element itself) that satisfies relation R, relation R is serial.-}

{- Test report:

Implemented a function isSerial to check whether a relation 
satisfies the serial (surjective) property over a given domain.

Definition: A relation R is serial over a domain A if and only if, 
    for all x ∈ A, there exists y ∈ A such that (x,y) ∈ R. 
    To verify the correctness of the implementation, 
    the following three properties are defined and tested using QuickCheck:

prop_definition_serial: Directly verifies that isSerial is 
equivalent to "for any x ∈ A, there exists y ∈ A such that (x,y) ∈ R" based on the definition.

prop_monotone: Monotonicity of a relation. 
If R is serial over A, then adding additional edges to R does not break the seriality.

prop_emptyDomain: Over an empty domain, 
a relation satisfies vacuously serial.

prop_definition_serial: All passed
prop_emptyDomain: All passed
prop_monotone: Some failed. This isn't due to an implementation error, 
but rather to the fact that the relation data automatically generated 
by QuickCheck doesn't necessarily meet the prerequisite (i.e., isSerial domain). 
Customizing an Arbitrary instance can resolve this issue. 



-}

module Exercise4 where

import Test.QuickCheck
import Data.List

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


-- Property 1: in the relation, every element of the domain 
--relates to another element of the domain
prop_definition_serial :: (Eq a, Show a) => [a] -> Rel a ->Bool
prop_definition_serial dom r = 
    isSerial dom r == all(\x -> any (\y -> (x,y) `elem` r) dom) dom


-- Property 2:If R is serial on A, then adding additional 
--edges to R does not destroy its serial property.
prop_monotone :: (Eq a, Show a) => [a] -> Rel a -> Rel a -> Property
prop_monotone dom r extra = isSerial dom r ==> isSerial dom(nub (r ++ extra)) 

-- Property 3: Null domain boundaries. 
--Serial defaults to true on empty domains.
prop_emptyDomain :: Rel Int -> Bool
prop_emptyDomain r = isSerial [] r == True




main :: IO ()
main = do
    quickCheck (prop_definition_serial :: [Int] -> Rel Int -> Bool)
    quickCheck (prop_monotone :: [Int] -> Rel Int -> Rel Int -> Property)
    quickCheck prop_emptyDomain
