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

-- Checks the list of row lengths - true if lengths are expected : expected +1 | expected etc.
checkLengthPattern :: [Int] -> Int -> Bool -> Bool
checkLengthPattern [x] expected _ = x == expected

checkLengthPattern (x:xs) expected True
    | x == expected = checkLengthPattern xs (expected+1) False
    | otherwise = False

checkLengthPattern (x:xs) expected False
    | x == expected = checkLengthPattern xs (expected-1) True
    | otherwise = False

-- Returns true, if all rows contain only legal characters, false otherwise
checkLettersInRow :: String -> Bool
checkLettersInRow xs = foldr (&&) True (map isAllowedLetter xs)

isAllowedLetter :: Char -> Bool
isAllowedLetter x = elem x ".ABCDEFG"

-- Puts a letter c at position x,y in a given honeycomb and returns the new honeycomb state
putLetter :: Honeycomb -> Char -> (Int, Int) -> Honeycomb
putLetter (Honeycomb hc) c (x, y) = Honeycomb ((take x hc) ++ [take y (hc!!x) ++ [c] ++ drop (y+1) (hc!!x)] ++ (drop (x+1) hc))

-- Returns a letter at position x,y-1
getLetter :: Honeycomb -> (Int, Int) -> Char
getLetter (Honeycomb hc) (x, y) = hc!!x!!y

-- Returns a list of all letters (including dots) surrounding a field (x,y), including (x,y) itself
getNeighbours :: Honeycomb -> (Int, Int) -> [Char]
getNeighbours (Honeycomb hc) (x,y) = map (getLetter (Honeycomb hc)) (getNeighboursPositions (Honeycomb hc) (x,y))

-- Returns a list of positions surrounding a field (x,y), including (x,y) itself
getNeighboursPositions :: Honeycomb -> (Int, Int) -> [(Int, Int)]
getNeighboursPositions (Honeycomb hc) (x, y) = [(sx, sy) | (sx, sy) <-
                                     (x-1, ny-1):(x-1,ny):
                                     (x,y-1):(x,y):(x,y+1):
                                     (x+1,ny-1):[(x+1,ny)],
                                     sx>=0, x< (length hc), sy>=0, sy<(length (hc!!sx))]
                                     where ny = if even x then y+1 else y

-- Returns a list of all letters that can be put in a given slot
getPossibleLetters :: Honeycomb -> (Int, Int) -> [Char]
getPossibleLetters (Honeycomb hc) (x, y) = [c | c <- "ABCDEFG", notInNeighbourhoods c (map (getNeighbours (Honeycomb hc)) (getNeighboursPositions (Honeycomb hc) (x,y)))]

-- Returns true if a char is absent from all sequences, false otherwise
notInNeighbourhoods :: Char -> [[Char]] -> Bool
notInNeighbourhoods c [] = True
notInNeighbourhoods c (x:xs) = if not (elem c x) then notInNeighbourhoods c xs else False