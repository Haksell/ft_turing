{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (isSuffixOf)
import Machine (parseMachine, validateMachine)
import System.Environment (getArgs, getProgName)

import Data.ByteString.Lazy qualified as BL

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFilePath, _input] ->
      if ".json" `isSuffixOf` jsonFilePath
        then do
          jsonFileContents <- BL.readFile jsonFilePath
          case parseMachine jsonFileContents of
            Left err -> putStrLn $ "Error parsing JSON: " ++ err
            Right machine ->
              case validateMachine machine of
                Left verr -> putStrLn $ "Validation error: " ++ verr
                Right validMachine -> print validMachine
        else putStrLn "Error: the file path must end with '.json'."
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " machine.json input"
      putStrLn "machine.json\tjson description of the machine"
      putStrLn "input\t\tinput of the machine"
