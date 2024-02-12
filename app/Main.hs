{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Machine (Machine (alphabet, blank), parseMachine, printMachine, validateMachine)
import System.Environment (getArgs, getProgName)

isValidInput :: [String] -> String -> String -> Either String ()
isValidInput machineAlphabet machineBlank input
  | null input = Left "Input is empty"
  | machineBlank `elem` inputSymbols = Left "Input contains the blank symbol"
  | not (all (`elem` machineAlphabet) inputSymbols) = Left "Input contains symbols not in alphabet"
  | otherwise = Right ()
 where
  inputSymbols = map (: []) input

doJob :: Machine -> String -> IO ()
doJob machine input = do
  print input
  putStrLn $ printMachine machine

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFilePath, input] ->
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
                    Right _ -> doJob validMachine input
        else putStrLn "Error: the file path must end with '.json'."
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " machine.json input"
      putStrLn "machine.json\tjson description of the machine"
      putStrLn "input\t\tinput of the machine"
