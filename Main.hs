module Main where
import RiddleSolver

main = do putStrLn "Podaj ścieżkę do pliku z łamigłówką:"
          fileName <- getLine
          file <- readFile fileName
          if isCorrect (read file :: Honeycomb) == False
            then putStrLn "Podany plik nie jest poprawny."
          else putStrLn ("Rozwiązanie: " ++ show (solve (read file :: Honeycomb)))