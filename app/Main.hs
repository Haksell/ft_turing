{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate, isSuffixOf, nub, sort)
import Data.Map.Strict as Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Executor
import Machine (Machine (..), buildMachine, printMachine)
import Options.Applicative
  ( Parser,
    ReadM,
    argument,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    readerError,
    short,
    str,
    switch,
    (<**>),
  )

defaultMaxSteps :: Integer
defaultMaxSteps = 10000

isValidInput :: [Char] -> Char -> String -> Maybe String
isValidInput machineAlphabet machineBlank input
  | machineBlank `elem` input = Just "Input contains the blank symbol"
  | not $ null unknownChars = Just $ "Input contains symbols not in the alphabet: " ++ intercalate ", " (map (: []) (sort $ nub unknownChars))
  | otherwise = Nothing
  where
    unknownChars = filter (`notElem` machineAlphabet) input

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

createTape :: String -> Tape
createTape input = fromList $ enumerate input

ftTuring :: Machine -> String -> Bool -> Integer -> IO ()
ftTuring machine input debug maxSteps = do
  when debug $ putStrLn $ printMachine machine
  (finalTape, finalPos, finalMessage) <-
    execute
      debug
      maxSteps
      maxSteps
      machine
      (createTape input)
      (initial machine)
      0
      Set.empty
  putStrLn $ stringifyTape finalTape finalPos (blank machine) ++ " " ++ finalMessage

data CommandLineArgs = CommandLineArgs
  { argJsonFilePath :: String,
    argInput :: String,
    argQuiet :: Bool,
    argMaxSteps :: Maybe Integer
  }

positiveInteger :: ReadM Integer
positiveInteger = do
  value <- auto
  if value > 0
    then return value
    else readerError "max-steps must be a positive integer"

parseCommandLineArgs :: Parser CommandLineArgs
parseCommandLineArgs =
  CommandLineArgs
    <$> argument str (metavar "machine.json" <> help "json description of the machine")
    <*> argument str (metavar "input" <> help "input of the machine")
    <*> switch (long "quiet" <> short 'q' <> help "only show final tape")
    <*> optional (option positiveInteger (long "max-steps" <> short 'm' <> metavar "n" <> help "maximum number of iterations (must be positive)"))

main :: IO ()
main = do
  args <- execParser $ info (parseCommandLineArgs <**> helper) fullDesc
  let jsonFilePath = argJsonFilePath args
  let input = argInput args
  let debug = not $ argQuiet args
  let maxSteps = fromMaybe defaultMaxSteps $ argMaxSteps args
  if ".json" `isSuffixOf` jsonFilePath
    then do
      jsonFileContents <- BL.readFile jsonFilePath
      case buildMachine jsonFileContents of
        Left parsingError -> putStrLn parsingError
        Right machine ->
          case isValidInput (alphabet machine) (blank machine) input of
            Just err -> putStrLn $ "Error: " ++ err
            Nothing -> ftTuring machine input debug maxSteps
    else putStrLn "Error: the file path must end with '.json'."