-- Time spent: 180 min

{-
This file contains the solution for finding the thief. It finds out who accuses who,
generates a list of accusers per person, and then finds the person who is accused by exactly
three people, as the thief is accused by the three people who are speaking the truth.
We can verify that the solution is correct by the fact it finds a person who is accused three times,
and does not find any other people meeting these criteria.
-}

module Exercise5 where

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool

-- Matthew: says he and Carl didn't do it, so accuses the rest
accuses Matthew target = target /= Carl && target /= Matthew

-- Peter: straightforward: Matthew or Jack
accuses Peter target = target == Matthew || target == Jack

-- Jack: not straightforward, accuses whomever Matthew and Peter don't accuse
accuses Jack target = not (accuses Matthew target) && not (accuses Peter target)

-- Arnold: XOR of Matthew and Peter
accuses Arnold target = accuses Matthew target /= accuses Peter target

-- Carl: reverse of Arnold
accuses Carl target = not (accuses Arnold target)

-- List of accusers for each boy
accusers :: Boy -> [Boy]
accusers target = [accuser | accuser <- boys, accuses accuser target]

-- Finds the thief
-- Could take the head directly but then type checker complains about possibly empty list
-- Searches for length 3 because three boys are speaking the thruth, and thus directly or indirectly accuse the thief
guilty :: Boy
guilty = case candidates of
        [] -> error "No guilty person found"
        (x:_) -> x
    where candidates = [target | target <- boys, length (accusers target) == 3]

main :: IO ()
main = do
    print (show guilty)
