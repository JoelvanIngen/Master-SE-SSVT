-- Time spent: 300 min

{-

1. How can you prove that the sub implementation is correct?
   - For a correct sub implementation, it must contain all (and only the) sub-formulae of f
   - We will use the properties:
     - The subformulae of Prop x include itself
     - For Neg x, the subformulae are itself and those of f



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

-- Property 2: The size of the set from sub matches the count from nsub
prop_nsubCorrect :: Form -> Bool
prop_nsubCorrect f = nsub f == setSize (sub f)
  where
    setSize (Set xs) = length xs

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
    quickCheck prop_selfInSub
    quickCheck prop_nsubCorrect
