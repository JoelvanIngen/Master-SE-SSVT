-- Time spent: 300 min

{-

- How can you prove that the sub implementation is correct?
   - For a correct sub implementation, it must contain all (and only the) sub-formulae of f
   - We will use the properties:
     - The subformulae of Prop x include itself
     - Only actual sub-formulae are present, and no duplicates

- Test for nsub
   - We assure that the size of the Form matches the upper bound determined by
         the nsub function.

Test reports:
- Tests are conducted on both the `sub` and `nsub` functions, as described above.
- All tests pass without problems.

-}

module Exercise7 where

import Test.QuickCheck
import SetOrd

type Name = Int

data Form = Prop Name
    | Neg Form
    | Cnj [Form]
    | Dsj [Form]
    | Impl Form Form
    | Equiv Form Form
    deriving (Eq, Ord, Show)

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- Recursive length counting
nsub :: Form -> Int
nsub (Prop _)       = 1
nsub (Neg f)        = 1 + nsub f
nsub (Cnj [f1,f2])  = 1 + nsub f1 + nsub f2
nsub (Dsj [f1,f2])  = 1 + nsub f1 + nsub f2
nsub (Impl f1 f2)   = 1 + nsub f1 + nsub f2
nsub (Equiv f1 f2)  = 1 + nsub f1 + nsub f2

-- Property 1: A formula is in its own set of sub-formulae
prop_selfInSub :: Form -> Bool
prop_selfInSub f = inSet f (sub f)

-- Helper function to get the list of immediate sub-components of a formula
immediateChildren :: Form -> [Form]
immediateChildren (Prop _)        = []
immediateChildren (Neg f)         = [f]
immediateChildren (Cnj [f1,f2])   = [f1, f2]
immediateChildren (Dsj [f1,f2])   = [f1, f2]
immediateChildren (Impl f1 f2)    = [f1, f2]
immediateChildren (Equiv f1 f2)   = [f1, f2]

-- Property 2: All children must be in its set of subformulae
prop_componentInSub :: Form -> Property
prop_componentInSub f = property $ all (\child -> inSet child (sub f)) (immediateChildren f)

-- Test for nsub: The size of the set from sub matches the count from nsub
prop_nsubCorrect :: Form -> Bool
prop_nsubCorrect f = nsub f == setSize (sub f)

-- Helper to get the size of a Set.
setSize :: Set a -> Int
setSize (Set xs) = length xs

-- Number of nodes in the Form AST must be greater or equal to number of
-- unique subformulae
prop_nsub_is_geq_setSize_sub :: Form -> Bool
prop_nsub_is_geq_setSize_sub f = nsub f >= setSize (sub f)


instance Arbitrary Form where
    arbitrary = sized arbForm

-- Create random Forms with a set size
-- Generates/extends formulas with the options being:
--  - Prop
--  - Neg x
--  - Conjuction x y
--  - Disjunction x y
--  - Implication x y
--  - Equivalence x y
-- Uses n as counter, ensuring finite formula with pre-defined length
arbForm :: Int -> Gen Form
arbForm 0 = fmap Prop arbitrary
arbForm n = oneof
  [ fmap Prop arbitrary
  , fmap Neg (arbForm (n-1))
  , do f1 <- arbForm (n `div` 2)
       f2 <- arbForm (n `div` 2)
       oneof [ pure (Cnj [f1,f2])
             , pure (Dsj [f1,f2])
             , pure (Impl f1 f2)
             , pure (Equiv f1 f2) ]
  ]

main :: IO ()
main = do
    -- Test sub properties
    quickCheck prop_selfInSub
    quickCheck prop_componentInSub

    -- Test nsub properties
    quickCheck prop_nsub_is_geq_setSize_sub
    