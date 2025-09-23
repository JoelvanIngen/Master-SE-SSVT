-- Time spent: 180 min

{- Test report:

-}

module Exercise6 (main) where

import Data.List
import Test.QuickCheck

type Name = Int
data Form = Prop Name
    | Neg Form
    | Cnj [Form]
    | Dsj [Form]
    | Impl Form Form
    | Equiv Form Form
    deriving (Eq,Ord)

{-
To convert any formula into CNF form the following order of steps is needed:
Remove implication and equivalence (remove arrows):
- P --> Q -> ¬P ∨ Q
- P <--> Q -> (P ∧ Q) ∨ (¬P ∧ ¬Q)
Then, convert to negation normal form (logically equivalent, but split props into negations):
- P -> ¬(¬P)
- ¬(P ∨ Q) -> ¬P ∧ ¬Q and reverse for ∨/∧
Then, use distributive law:
- (P ∨ (Q ∧ R)) -> (P ∨ Q) ∧ (P ∨ R) (but not reverse for ∨/∧, since this is CNF not DNF)
This makes a logically equivalent formula that exists only of conjuctions of disjunctions of atoms:
(P ∨ Q) ∧ (P ∨ R) ∧ ...
-}

-- Replaces implications and equivalences with logical equivalences
arrowfree :: Form -> Form
arrowfree (Prop x) = Prop x
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
    where
        f1' = arrowfree f1
        f2' = arrowfree f2

-- Converts to negation normal form
nnf :: Form -> Form
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf . Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf . Neg) fs)

-- TODO: Convert using distributive law ( (P ∨ (Q ∧ R)) -> (P ∨ Q) ∧ (P ∨ R) )
dlaw :: Form -> Form
dlaw (Prop x) = Prop x
dlaw (Neg f) = Neg (dlaw f)
dlaw (Cnj fs) = Cnj (map dlaw fs)
dlaw (Dsj fs) = Dsj (map dlaw fs)
dlaw (Dsj f1 (Cnj fs)) = {- dsj every part of f1 -}
-- Essentially here is every literal in P (f1) should be multiplied with Q ∧ R separately
-- So make (P1 ∨ (Q ∧ R)) ∧ etc. and after that do (P1 ∨ Q) ∧ (P1 ∨ R) ∧ etc.

p = Prop 1
q = Prop 2

cnf :: Form -> Form
cnf = dlaw . nnf . arrowfree -- TODO: Is this the right order?

main :: IO ()
main = do
    quickCheck cnf -- TODO: actually make test for this
