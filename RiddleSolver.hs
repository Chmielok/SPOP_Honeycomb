module RiddleSolver where
import Data.List (sortBy)

data Honeycomb = Honeycomb [String] deriving (Show, Read)

-- Applies 3 validations:
-- 1. Correct number of rows
-- 2. Correct number of elements in every row
-- 3. Contains only correct letters
isCorrect :: Honeycomb -> Bool
isCorrect (Honeycomb hc)
    | even (length hc) = False -- 1
    | checkLengthPattern (map length hc) (length (hc!!0)) True == False = False -- 2
    | foldr (&&) True (map checkLettersInRow hc) == False = False -- 3
    | otherwise = True -- validation passed

checkLengthPattern :: [Int] -> Int -> Bool -> Bool
checkLengthPattern [x] expected _ = x == expected

checkLengthPattern (x:xs) expected True
    | x == expected = checkLengthPattern xs (expected+1) False
    | otherwise = False

checkLengthPattern (x:xs) expected False
    | x == expected = checkLengthPattern xs (expected-1) True
    | otherwise = False

checkLettersInRow :: String -> Bool
checkLettersInRow xs = foldr (&&) True (map isAllowedLetter xs)

isAllowedLetter :: Char -> Bool
isAllowedLetter x = elem x ".ABCDEFG"