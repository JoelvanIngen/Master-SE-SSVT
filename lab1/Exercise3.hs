{-
Properties to test
• (\ x -> even x && x > 3) or even
• (\ x -> even x || x > 3) or even
• (\ x -> (even x && x > 3) || even x) or even
• even or (\ x -> (even x && x > 3) || even x)
-}

module Exercise3 where

domain :: [Integer]
domain = [-10..10]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall' :: [a] -> (a -> Bool) -> Bool
forall' = flip all

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall' xs (\ x -> p x --> q x)

p1, p2, p3, p4 :: Integer -> Bool

p1 n = even n && n > 3
p2 n = even n || n > 3
p3 n = (even n && n > 3) || even n
p4 n = (even n && n > 3) || even n

main :: IO ()
main = do
    putStrLn $ "p1 stronger than p2: " ++ show (stronger domain p1 p2)
    putStrLn $ "p2 stronger than p1: " ++ show (stronger domain p2 p1)
    putStrLn $ "p1 stronger than p3: " ++ show (stronger domain p1 p3)
    putStrLn $ "p3 stronger than p1: " ++ show (stronger domain p3 p1)
    putStrLn $ "p1 stronger than p4: " ++ show (stronger domain p1 p4)
    putStrLn $ "p4 stronger than p1: " ++ show (stronger domain p4 p1)
    putStrLn $ "p2 stronger than p3: " ++ show (stronger domain p2 p3)
    putStrLn $ "p3 stronger than p2: " ++ show (stronger domain p3 p2)
    putStrLn $ "p2 stronger than p4: " ++ show (stronger domain p2 p4)
    putStrLn $ "p4 stronger than p2: " ++ show (stronger domain p4 p2)
    putStrLn $ "p3 stronger than p4: " ++ show (stronger domain p3 p4)
    putStrLn $ "p4 stronger than p3: " ++ show (stronger domain p4 p3)
