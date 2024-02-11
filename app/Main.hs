{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Machine (parseMachine, validateMachine)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFilePath, _input] ->
      if ".json" `isSuffixOf` jsonFilePath
        then do
          jsonFileContents <- BL.readFile jsonFilePath
          case parseMachine jsonFileContents of
            Left parsingError -> putStrLn $ "Error parsing JSON: " ++ parsingError
            Right machine ->
              case validateMachine machine of
                Left validationError -> putStrLn $ "Validation error: " ++ validationError
                Right validMachine -> print validMachine
        else putStrLn "Error: the file path must end with '.json'."
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " machine.json input"
      putStrLn "machine.json\tjson description of the machine"
      putStrLn "input\t\tinput of the machine"
