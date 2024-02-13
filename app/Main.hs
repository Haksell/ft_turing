{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf, isSuffixOf, partition)
import Data.Map.Strict as Map (Map, empty, foldrWithKey, insert)
import GHC.Generics (Generic)
import Machine (Machine (alphabet, blank, finals, initial, name, states, transitions), Transition (action, read, to_state, write), Transitions, parseMachine, printMachine, validateMachine)
import System.Environment (getArgs, getProgName)
import Prelude hiding (read, write) -- TODO: fix this garbage

data TransitionValue = TransitionValue
  { toState :: String,
    writeGood :: String,
    isLeft :: Bool
  }
  deriving (Show, Generic)

type TransitionsGood = Map.Map (String, String) TransitionValue

data MachineGood = MachineGood
  { nameGood :: String,
    alphabetGood :: [String],
    blankGood :: String,
    statesGood :: [String],
    initialGood :: String,
    finalsGood :: [String],
    transitionsGood :: TransitionsGood
  }
  deriving (Show, Generic)

isValidInput :: [String] -> String -> String -> Either String ()
isValidInput machineAlphabet machineBlank input
  | machineBlank `elem` inputSymbols = Left "Input contains the blank symbol"
  | not (all (`elem` machineAlphabet) inputSymbols) = Left "Input contains symbols not in alphabet"
  | otherwise = Right ()
  where
    inputSymbols = map (: []) input

goodifyTransitions :: Transitions -> TransitionsGood
goodifyTransitions transitions = Map.foldrWithKey insertTransitions Map.empty transitions
  where
    insertTransitions :: String -> [Transition] -> TransitionsGood -> TransitionsGood
    insertTransitions stateKey transitions acc = foldr (insertTransition stateKey) acc transitions

    insertTransition :: String -> Transition -> TransitionsGood -> TransitionsGood
    insertTransition stateKey transition acc =
      let key = (stateKey, read transition)
          value =
            TransitionValue
              { toState = to_state transition,
                writeGood = write transition,
                isLeft = action transition == "LEFT"
              }
       in Map.insert key value acc

goodifyMachine :: Machine -> MachineGood
goodifyMachine machine =
  MachineGood
    { nameGood = name machine,
      alphabetGood = alphabet machine,
      blankGood = blank machine,
      statesGood = states machine,
      initialGood = initial machine,
      finalsGood = finals machine,
      transitionsGood = goodifyTransitions $ transitions machine
    }

doJob :: Machine -> String -> Bool -> IO ()
doJob machine _input debug = do
  let machineGood = goodifyMachine machine
  print $ transitionsGood machineGood
  when debug $ putStrLn $ printMachine machine
  putStrLn "TODO: FINAL TAPE"

ftTuring :: FilePath -> String -> Bool -> IO ()
ftTuring jsonFilePath input debug =
  if ".json" `isSuffixOf` jsonFilePath
    then do
      jsonFileContents <- BL.readFile jsonFilePath
      case parseMachine jsonFileContents of
        Left parsingError -> putStrLn $ "Error parsing JSON: " ++ parsingError
        Right machine ->
          case validateMachine machine of
            Left validationError -> putStrLn $ "Validation error: " ++ validationError
            Right validMachine ->
              case isValidInput (alphabet validMachine) (blank validMachine) input of
                Left err -> putStrLn $ "Error: " ++ err
                Right _ -> doJob validMachine input debug
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

main :: IO ()
main = do
  args <- getArgs
  let (flags, positional) = partition (isPrefixOf "-") args
  if any (`elem` flags) ["-h", "--help"]
    then printHelp
    else case positional of
      [jsonFilePath, input] -> ftTuring jsonFilePath input $ not $ any (`elem` flags) ["-q", "--quiet"]
      _ -> printHelp
