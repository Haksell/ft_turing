{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Machine (Machine (alphabet, blank), parseMachine, printMachine, validateMachine)
import System.Environment (getArgs, getProgName)

isValidInput :: [String] -> String -> String -> Either String ()
isValidInput machineAlphabet machineBlank input
  | machineBlank `elem` inputSymbols = Left "Input contains the blank symbol"
  | not (all (`elem` machineAlphabet) inputSymbols) = Left "Input contains symbols not in alphabet"
  | otherwise = Right ()
  where
    inputSymbols = map (: []) input

doJob :: Machine -> String -> Bool -> IO ()
doJob machine _input debug = do
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFilePath, input] -> do ftTuring jsonFilePath input True
    [jsonFilePath, input, "--quiet"] -> do ftTuring jsonFilePath input False
    [jsonFilePath, input, "-q"] -> do ftTuring jsonFilePath input False
    _ -> do
      progName <- getProgName
      putStrLn $ "usage: " ++ progName ++ " machine.json input [--quiet]"
      putStrLn "positional arguments:"
      putStrLn "    machine.json    json description of the machine"
      putStrLn "    input           input of the machine"
      putStrLn "optional arguments:"
      putStrLn "    -h, --help      show this help message and exit"
      putStrLn "    -q, --quiet     only show final tape"
