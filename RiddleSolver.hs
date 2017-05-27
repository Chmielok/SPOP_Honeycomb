module RiddleSolver where
import Data.List (sortBy)

data Honeycomb = Honeycomb [String] deriving (Show, Read)

isCorrect :: Honeycomb -> Bool
isCorrect hc = True