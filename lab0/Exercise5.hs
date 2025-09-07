--time spend 110 min 
--Quicktest generates Unicode, causing the test to fail. Added a filter condition

import Data.Char
import Test.QuickCheck

{- ROT13 is a monoalphabetic substitution cipher algorithm that shifts each letter in the alphabet by 13 positions 
(wrapping around from the end to the beginning), while leaving non-alphabetic characters unchanged. 
Its distinctive feature is that applying the encryption twice returns the original input, 
and the input and output maintain the same length.-}

rot13 :: [Char] -> [Char]
rot13 str = map characterType str

--
characterType :: Char -> Char
characterType t 
    | 'A' <= t && t <= 'Z'  = turnUpperChar t
    | 'a' <= t && t <= 'z'  = turnLowerChar t
    | otherwise = t


turnLowerChar :: Char -> Char
turnLowerChar lc = exchangeChar
    where
        original = ord lc
        differenceAsc1 = original - ord 'a'
        differenceAsc2 = (differenceAsc1 + 13) `mod` 26
        exchangeChar = chr(ord 'a' + differenceAsc2)


turnUpperChar :: Char -> Char
turnUpperChar uc = exchangeChar
    where
        original = ord uc
        differenceAsc1 = original - ord 'A'
        differenceAsc2 = (differenceAsc1 + 13) `mod` 26
        exchangeChar = chr(ord 'A' + differenceAsc2)

testLength :: [Char] -> Bool
testLength testChar = length testChar == length (rot13 testChar)

testSymmetry :: String  -> Bool
testSymmetry testChar = testChar == rot13 (rot13 testChar)


main :: IO ()
main = do
    quickCheck testLength
    quickCheck testSymmetry
  

