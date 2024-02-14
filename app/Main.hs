{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate, isPrefixOf, isSuffixOf, partition, sort)
import Data.Map.Strict as Map (Map, fromList, insert, lookup, member, toList)
import Data.Maybe (fromJust)
import Machine (Machine (..), Transition (..), buildMachine, printMachine)
import System.Environment (getArgs, getProgName)

type Tape = Map.Map Integer Char

maxSteps :: Integer
maxSteps = 10000 -- TODO user-defined (--max_steps=n)

isValidInput :: [Char] -> Char -> String -> Either String ()
isValidInput machineAlphabet machineBlank input
  | machineBlank `elem` input = Left "Input contains the blank symbol"
  | not (all (`elem` machineAlphabet) input) = Left "Input contains symbols not in the alphabet"
  | otherwise = Right ()

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

createTape :: String -> Tape
createTape input = fromList $ enumerate input

-- Anton: need to use unused variables?
-- TODO: return start when we'll try to detect loops
-- TODO: check (ChatGPT) (forgot to delete blanks at the end and start)
stringifyTape :: Tape -> Integer -> Char -> String
stringifyTape tape pos blankChar = "[" ++ concatMap showCell nonBlankCells ++ "]"
  where
    sortedCells = sort (Map.toList tape)
    filterBlanks = reverse . dropWhile (\(_, c) -> c == blankChar) . reverse . dropWhile (\(_, c) -> c == blankChar)
    showCell (k, v)
      | k == pos = "<" ++ [v] ++ ">"
      | otherwise = [v]
    nonBlankCells = filterBlanks sortedCells

-- Better functions names for execute, prepareAndExecute, openFileAndExecute

execute :: Bool -> Integer -> Machine -> Tape -> String -> Integer -> IO (Tape, String)
execute debug remainingSteps machine tape state pos
  | state `elem` finals machine = return (tape, "Final state: " ++ state)
  | remainingSteps == 0 = return (tape, "No final state found after " ++ show maxSteps ++ " steps")
  | otherwise = do
      let machineBlank = blank machine
      let newTape = if pos `member` tape then tape else insert pos machineBlank tape
      let cell = fromJust $ Map.lookup pos newTape
      case Map.lookup (state, cell) $ transitions machine of
        Nothing -> return (newTape, "Unexpected transition: (" ++ state ++ ", " ++ [cell] ++ ")")
        Just tv -> do
          when debug $ putStrLn $ stringifyTape newTape pos machineBlank ++ " (" ++ state ++ ", " ++ [cell] ++ ") -> (" ++ toState tv ++ ", " ++ [writeChar tv] ++ ", " ++ (if isLeft tv then "LEFT" else "RIGHT") ++ ")"
          execute debug (remainingSteps - 1) machine (insert pos (writeChar tv) newTape) (toState tv) (if isLeft tv then pos - 1 else pos + 1)

-- TODO: completeState stuff

prepareAndExecute :: Machine -> String -> Bool -> IO ()
prepareAndExecute machine input debug = do
  when debug $ putStrLn $ printMachine machine
  (finalTape, finalMessage) <- execute debug maxSteps machine (createTape input) (initial machine) 0
  putStrLn $ stringifyTape finalTape (-42) (blank machine) ++ " " ++ finalMessage

openFileAndExecute :: FilePath -> String -> Bool -> Integer -> IO ()
openFileAndExecute jsonFilePath input debug _operationsLimit =
  if ".json" `isSuffixOf` jsonFilePath
    then do
      jsonFileContents <- BL.readFile jsonFilePath
      case buildMachine jsonFileContents of
        Left parsingError -> putStrLn parsingError
        Right machine ->
          case isValidInput (alphabet machine) (blank machine) input of
            Left err -> putStrLn $ "Error: " ++ err
            Right _ -> prepareAndExecute machine input debug
    else putStrLn "Error: the file path must end with '.json'."

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "usage: " ++ progName ++ " machine.json input [--quiet]"
  putStrLn "positional arguments:"
  putStrLn "    machine.json    json description of the machine"
  putStrLn "    input           input of the machine"
  putStrLn "optional arguments:"
  putStrLn "    -h, --help      show this help message and exit"
  putStrLn "    -q, --quiet     only show final tape"

helpFlagAlternatives :: [String]
helpFlagAlternatives = ["-h", "--help"]

quietFlagAlternatives :: [String]
quietFlagAlternatives = ["-q", "--quiet"]

allFlagAlternatives :: [String]
allFlagAlternatives = helpFlagAlternatives ++ quietFlagAlternatives

hasFlag :: [String] -> [String] -> Bool
hasFlag flags = any (`elem` flags)

main :: IO ()
main = do
  args <- getArgs
  let (flags, positional) = partition (isPrefixOf "-") args
  if hasFlag flags helpFlagAlternatives
    then printHelp
    else do
      let unknownFlags = filter (`notElem` allFlagAlternatives) flags
      if null unknownFlags
        then case positional of
          [jsonFilePath, input] -> openFileAndExecute jsonFilePath input (not $ hasFlag flags quietFlagAlternatives) 10000
          _ -> do
            putStrLn $ "expected 2 positional arguments, got " ++ show (length positional)
            printHelp
        else do
          putStrLn $ "unknown flag" ++ (if length unknownFlags == 1 then "" else "s") ++ ": " ++ intercalate ", " unknownFlags
          printHelp
