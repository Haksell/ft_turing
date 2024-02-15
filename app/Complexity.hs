module Complexity where

import qualified Control.Monad
import qualified Data.Set as Set
import Executor (createTape, execute)
import Machine (Machine (..), isValidInput)
import System.Random (randomRIO)

chooseRandom :: Int -> String -> IO String
chooseRandom k str = Control.Monad.replicateM k (randomChoice str)

randomChoice :: String -> IO Char
randomChoice str
  | null str = error "Error, '[]' is detected, pattern is incorrect."
  | otherwise = do
      randomIndex <- randomRIO (0, length str - 1)
      return $ str !! randomIndex

generateInputByPattern :: String -> Int -> IO String
generateInputByPattern pattern size = go pattern ""
 where
  go [] acc = return acc
  go ('[' : xs) acc = do
    let (choiceStr, rest) = break (== ']') xs
    if null rest
      then error "Malformed pattern: missing closing ']'"
      else do
        let remaining = tail rest
        chosen <- chooseRandom size choiceStr
        go remaining (acc ++ chosen)
  go (x : xs) acc
    | x == ']' = go xs acc
    | otherwise = go xs (acc ++ [x])

executeMachine :: Machine -> String -> IO Integer
executeMachine machine input = do
  print input
  (_finalTape, _finalPos, _finalMessage, stepsCount) <- execute False 500000 500000 machine (createTape input) (initial machine) 0 Set.empty
  return stepsCount

printRoughComplexity :: [(Int, Integer)] -> IO ()
printRoughComplexity = mapM_ print

calculateComplexity :: String -> Machine -> IO ()
calculateComplexity pattern machine = do
  let sizes = [2 .. 24]
  results <-
    mapM
      ( \size -> do
          inp <- generateInputByPattern pattern size
          case isValidInput (alphabet machine) (blank machine) inp of
            Just err -> do
              print err
              return (length inp, 0)
            Nothing -> do
              steps <- executeMachine machine inp
              return (length inp, steps)
      )
      sizes
  printRoughComplexity results
