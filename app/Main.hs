{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf, sort)
import Data.Map.Strict as Map (Map, fromList, insert, lookup, member, toList)
import Data.Maybe (fromJust, fromMaybe)
import Machine (Machine (..), Transition (..), buildMachine, printMachine)
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

type Tape = Map.Map Integer Char

defaultMaxSteps :: Integer
defaultMaxSteps = 10000

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

execute :: Bool -> Integer -> Integer -> Machine -> Tape -> String -> Integer -> IO (Tape, String)
execute debug maxSteps remainingSteps machine tape state pos
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
          execute debug maxSteps (remainingSteps - 1) machine (insert pos (writeChar tv) newTape) (toState tv) (if isLeft tv then pos - 1 else pos + 1)

-- TODO: completeState stuff

prepareAndExecute :: Machine -> String -> Bool -> Integer -> IO ()
prepareAndExecute machine input debug maxSteps = do
  when debug $ putStrLn $ printMachine machine
  (finalTape, finalMessage) <- execute debug maxSteps maxSteps machine (createTape input) (initial machine) 0
  putStrLn $ stringifyTape finalTape (-42) (blank machine) ++ " " ++ finalMessage

openFileAndExecute :: FilePath -> String -> Bool -> Integer -> IO ()
openFileAndExecute jsonFilePath input debug maxSteps =
  if ".json" `isSuffixOf` jsonFilePath
    then do
      jsonFileContents <- BL.readFile jsonFilePath
      case buildMachine jsonFileContents of
        Left parsingError -> putStrLn parsingError
        Right machine ->
          case isValidInput (alphabet machine) (blank machine) input of
            Left err -> putStrLn $ "Error: " ++ err
            Right _ -> prepareAndExecute machine input debug maxSteps
    else putStrLn "Error: the file path must end with '.json'."

data CommandLineArgs = CommandLineArgs
  { argMaxSteps :: Maybe Integer,
    argQuiet :: Bool,
    argJsonFilePath :: String,
    argInput :: String
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
    <$> optional (option positiveInteger (long "max-steps" <> short 'm' <> metavar "n" <> help "maximum number of iterations (must be positive)"))
    <*> switch (long "quiet" <> short 'q' <> help "only show final tape")
    <*> argument str (metavar "machine.json" <> help "json description of the machine")
    <*> argument str (metavar "input" <> help "input of the machine")

main :: IO ()
main = do
  args <- execParser $ info (parseCommandLineArgs <**> helper) fullDesc
  openFileAndExecute (argJsonFilePath args) (argInput args) (not $ argQuiet args) (fromMaybe defaultMaxSteps $ argMaxSteps args)