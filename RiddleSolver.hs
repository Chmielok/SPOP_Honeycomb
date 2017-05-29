module RiddleSolver where
import Data.List (sortBy)

data Honeycomb = Honeycomb [String] deriving (Show, Read, Eq)

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

-- Returns a letter at position x,y
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
                                     sx>=0, sx< (length hc), sy>=0, sy<(length (hc!!sx))]
                                     where ny = if even x then y+1 else y

-- Returns a list of all letters that can be put in a given slot
getPossibleLetters :: Honeycomb -> (Int, Int) -> [Char]
getPossibleLetters (Honeycomb hc) (x, y) = [c | c <- "ABCDEFG", notInNeighbourhoods c (map (getNeighbours (Honeycomb hc)) (getNeighboursPositions (Honeycomb hc) (x,y)))]

-- Returns true if a char is absent from all sequences, false otherwise
notInNeighbourhoods :: Char -> [[Char]] -> Bool
notInNeighbourhoods c [] = True
notInNeighbourhoods c (x:xs) = if not (elem c x) then notInNeighbourhoods c xs else False

-- Returns a list of all empty fields in a honeycomb
getEmptyFields :: Honeycomb -> [(Int, Int)]
getEmptyFields (Honeycomb hc) = [(x,y) | (x,y) <- getAllFields (Honeycomb hc), getLetter (Honeycomb hc) (x,y) == '.']
                                where getAllFields (Honeycomb hc) = [(x,y)| x <- [0..(length hc - 1)], y <- [0..(length (hc!!x) -1)]]

-- Iterates over a honeycomb, filling all single-option gaps until either no change is made or all fields are filled
fillSingleGaps :: Honeycomb -> Honeycomb
fillSingleGaps (Honeycomb hc) | null emptyFields = (Honeycomb hc)
                              | filled == (Honeycomb hc) = (Honeycomb hc)
                              | otherwise = fillSingleGaps filled
                           where emptyFields = getEmptyFields (Honeycomb hc)
                                 filled = fillSingleGapsStep (Honeycomb hc) emptyFields

-- Fills all single-option empty gaps in one step - more single-option gaps might be found after that
fillSingleGapsStep :: Honeycomb -> [(Int, Int)] -> Honeycomb
fillSingleGapsStep (Honeycomb hc) [] = (Honeycomb hc)
fillSingleGapsStep (Honeycomb hc) (x:xs) | length possible == 1 = fillSingleGapsStep (putLetter (Honeycomb hc) (possible!!0) x) xs
                                         | otherwise = fillSingleGapsStep (Honeycomb hc) xs
                                    where possible = getPossibleLetters (Honeycomb hc) x

-- Tries to solve the riddle. If it's not possible, it show an error message
solve :: Honeycomb -> Honeycomb
solve hc = case solveFiltered (fillSingleGaps hc) of
    (_, False) -> error "brak rozwiązania"
    (solution, True) -> solution

-- Actual solving happens here. The honeycomb passed to this method is cleaned beforehand from obvious cells.
-- If the honeycomb is already solved, it is returned automatically
-- If not, the "guessing magic" (backtracking algorithm) starts
solveFiltered :: Honeycomb -> (Honeycomb, Bool)
solveFiltered hc
    | getEmptyFields hc == [] = (hc, True)
    | otherwise = fill hc sortedEmptyFields lettersPossibleForFirstField
    where sortedEmptyFields = sortBy (\x y -> compare (length (getPossibleLetters hc x)) (length (getPossibleLetters hc y))) (getEmptyFields hc)
          lettersPossibleForFirstField = getPossibleLetters hc (head sortedEmptyFields)

-- This function runs recursively and tries to fill all the fields, that were not previously filled.
-- It runs through a list of empty fields filling them with a letter. If the added letter makes the puzzle unsolvable, it goes back and tries a different letter.
-- If it runs out of letters, a 'False' message is passed, and the previous cell is informed it should try to be filled with another letter.
-- If there is a letter that fits the last cell, the puzzle is solved and 'True' message is passed.
-- If the first cell returns 'False' message, the riddle is unsolvable.
fill :: Honeycomb -> [(Int, Int)] -> [Char] -> (Honeycomb, Bool)
fill hc _ [] = (hc, False)
fill hc [cell] (letter:letters) = (putLetter hc letter cell, True)
fill hc (cell:nextCell:cells) (letter:letters)
    | putAndCheck == [] = fill hc (cell:nextCell:cells) letters
    | otherwise = case fill put (nextCell:cells) putAndCheck of
        (newHc, True) -> (newHc, True)
        (newHc, False) -> fill hc (cell:nextCell:cells) letters
    where put = putLetter hc letter cell
          putAndCheck = getPossibleLetters put nextCell