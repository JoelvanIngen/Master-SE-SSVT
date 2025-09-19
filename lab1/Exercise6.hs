-- Time spent: x min.

module Exercise6 where

import Data.List

type Name = Int
data Form = Prop Name
    | Neg Form
    | Cnj [Form]
    | Dsj [Form]
    | Impl Form Form
    | Equiv Form Form
    deriving (Eq,Ord)

-- cnf :: Form -> Form
-- cnf = 
    -- Here, do a different action per type of proposition
    -- P --> Q -> !P v Q
    -- P <--> Q -> (P ^ Q) v (!P ^ !Q)
    -- Then
    -- double negate simple prop !(!P)
    -- !(P v Q) -> !P ^ !Q and reverse for v/^
    -- Then, according to several sources
    -- (P v (Q ^ R)) -> (P v Q) ^ (P v R) (but not reverse for v/^?)

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

-- TODO: Convert using distributive law ( (P v (Q ^ R)) -> (P v Q) ^ (P v R) )
dlaw :: Form -> Form
dlaw (Prop x) = Prop x
dlaw (Neg f) = Neg (dlaw f)
dlaw (Cnj fs) = Cnj (map dlaw fs)
dlaw (Dsj fs) = Dsj (map dlaw fs)
dlaw (Dsj f1 (Cnj fs)) = {- dsj every part of f1 -}
-- Essentially here is every part of P (f1) should be multiplied with Q ^ R separately
-- So make P1 v (Q ^ R) etc. and after that do (P1 v Q) ^ (P1 v R) etc.

p = Prop 1
q = Prop 2

-- main :: IO ()
-- main = do
