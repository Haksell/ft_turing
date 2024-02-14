{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf, sort)
import Data.Map.Strict as Map (Map, fromList, insert, lookup, member, toList)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Set qualified as Set
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

type CompleteState = (String, String, Maybe Integer)

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

stringifyTapeWithPos :: Tape -> Integer -> Char -> (String, Maybe Integer)
stringifyTapeWithPos tape pos blankChar =
  ( "[" ++ concatMap showCell nonBlankCells ++ "]",
    listToMaybe $ map fst nonBlankCells
  )
  where
    sortedCells = sort (Map.toList tape)
    dropStartBlanks = dropWhile (\(_, c) -> c == blankChar)
    filterBlanks = reverse . dropStartBlanks . reverse . dropStartBlanks
    showCell (k, v) = if k == pos then "<" ++ [v] ++ ">" else [v]
    nonBlankCells = filterBlanks sortedCells

stringifyTape :: Tape -> Integer -> Char -> String
stringifyTape tape pos blankChar = fst $ stringifyTapeWithPos tape pos blankChar

execute :: Bool -> Integer -> Integer -> Machine -> Tape -> String -> Integer -> Set.Set CompleteState -> IO (Tape, Integer, String)
execute debug maxSteps remainingSteps machine tape state pos visitedStates
  | state `elem` finals machine = return (tape, pos, "Final state: " ++ state)
  | remainingSteps == 0 = return (tape, pos, "No final state found after " ++ show maxSteps ++ " steps")
  | otherwise = do
      let blankChar = blank machine
      let (strTape, start) = stringifyTapeWithPos tape pos blankChar
      let completeState = (state, strTape, fmap (pos -) start)
      if Set.member completeState visitedStates
        then return (tape, pos, "Infinite loop detected at state: " ++ state)
        else do
          let newTape = if pos `member` tape then tape else insert pos blankChar tape
          let cell = fromJust $ Map.lookup pos newTape
          case Map.lookup (state, cell) $ transitions machine of
            Nothing -> return (newTape, pos, "Unexpected transition: (" ++ state ++ ", " ++ [cell] ++ ")")
            Just tv -> do
              when debug $
                putStrLn $
                  stringifyTape newTape pos blankChar
                    ++ " ("
                    ++ state
                    ++ ", "
                    ++ [cell]
                    ++ ") -> ("
                    ++ toState tv
                    ++ ", "
                    ++ [writeChar tv]
                    ++ ", "
                    ++ (if isLeft tv then "LEFT" else "RIGHT")
                    ++ ")"
              execute
                debug
                maxSteps
                (remainingSteps - 1)
                machine
                (insert pos (writeChar tv) newTape)
                (toState tv)
                (if isLeft tv then pos - 1 else pos + 1)
                (Set.insert completeState visitedStates)

-- TODO: completeState stuff

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
            Left err -> putStrLn $ "Error: " ++ err
            Right _ -> ftTuring machine input debug maxSteps
    else putStrLn "Error: the file path must end with '.json'."