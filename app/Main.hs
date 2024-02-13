{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Aeson (defaultOptions, eitherDecode)
import Data.Aeson.Types (FromJSON (parseJSON), Options, genericParseJSON, rejectUnknownFields)
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (lefts)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, partition, sort)
import Data.Map.Strict as Map (Map, empty, foldrWithKey, fromList, insert, lookup, member, toList)
-- TODO: fix this garbage
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import System.Environment (getArgs, getProgName)
import Prelude hiding (read)

data TransitionValue = TransitionValue
  { toState :: String,
    writeGood :: String, -- TODO: char
    isLeft :: Bool
  }
  deriving (Show, Generic)

type TransitionsGood = Map.Map (String, Char) TransitionValue

data MachineGood = MachineGood
  { nameGood :: String,
    alphabetGood :: [String],
    blankGood :: Char,
    statesGood :: [String],
    initialGood :: String,
    finalsGood :: [String],
    transitionsGood :: TransitionsGood
  }
  deriving (Show, Generic)

type Tape = Map.Map Integer Char

data Transition = Transition
  { read :: String,
    to_state :: String, -- TODO: toState
    write :: String, -- TODO: char
    action :: String
  }
  deriving (Show, Generic)

type Transitions = Map.Map String [Transition]

data Machine = Machine
  { name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Transitions
  }
  deriving (Show, Generic)

instance FromJSON Machine where
  parseJSON = genericParseJSON $ aesonOptions "Machine"

instance FromJSON Transition where
  parseJSON = genericParseJSON $ aesonOptions "Transition"

lineSize :: Int
lineSize = 80

borderLine :: String
borderLine = replicate lineSize '*'

paddingLine :: String
paddingLine = "*" ++ replicate (lineSize - 2) ' ' ++ "*"

centerString :: Int -> Char -> String -> String
centerString width padChar s = leftPad ++ s ++ rightPad
  where
    (leftPadSize, parity) = (width - length s) `divMod` 2
    leftPad = replicate leftPadSize padChar
    rightPad = replicate (leftPadSize + parity) padChar

commaSeparated :: String -> String -> [String] -> String
commaSeparated start end xs = start ++ intercalate ", " xs ++ end

showLikeTuple :: [String] -> String
showLikeTuple = commaSeparated "(" ")"

showLikeArray :: [String] -> String
showLikeArray = commaSeparated "[" "]"

printMachine :: Machine -> String -- TODO: MachineGood to simplify transitions
printMachine machine =
  intercalate
    "\n"
    [ borderLine,
      paddingLine,
      "*" ++ centerString (lineSize - 2) ' ' (name machine) ++ "*",
      paddingLine,
      borderLine,
      "Alphabet: " ++ showLikeArray (alphabet machine),
      "States: " ++ showLikeArray (states machine),
      "Initial: " ++ initial machine,
      "Finals: " ++ showLikeArray (finals machine),
      printTransitions (transitions machine),
      borderLine
    ]

printTransitions :: Transitions -> String
printTransitions t = intercalate "\n" $ concatMap printStateTransitions (toList t)

printStateTransitions :: (String, [Transition]) -> [String]
printStateTransitions (state, ts) = map (printTransition state) ts

printTransition :: String -> Transition -> String
printTransition state transition =
  showLikeTuple [state, read transition]
    ++ " -> "
    ++ showLikeTuple [to_state transition, write transition, action transition]

aesonOptions :: String -> Options
aesonOptions _ = defaultOptions {rejectUnknownFields = True}

parseMachine :: B.ByteString -> Either String Machine
parseMachine = eitherDecode

validateMachine :: Machine -> Either String Machine
validateMachine machine
  | null (name machine) = Left "Machine name can't be empty"
  | length (alphabet machine) < 2 = Left "Alphabet len should be at least 2 characters"
  | any ((/= 1) . length) (alphabet machine) = Left "All elements in the alphabet must be a single character"
  | length (nub (alphabet machine)) /= length (alphabet machine) = Left "Alphabet must contain unique elements"
  | blank machine `notElem` alphabet machine = Left "Blank should be in the alphabet"
  | length (states machine) < 2 = Left "At least 2 states are expected (initial, finals)"
  | any null (states machine) = Left "State name can't be empty"
  | length (nub (states machine)) /= length (states machine) = Left "States names should be unique"
  | initial machine `notElem` states machine = Left "Initial state is not in the list of states."
  | null (finals machine) = Left "At least 1 finals expected."
  | not (all (`elem` states machine) (finals machine)) = Left "Not all final states are in the list of states."
  | length (nub (finals machine)) /= length (finals machine) = Left "Duplicates in finals doesn't make sense."
  | initial machine `elem` finals machine = Left "Initial value is equal to finals, program will exit immediately."
  | otherwise = areTransitionsValid (transitions machine) machine >> Right machine

isValidTransition :: Transition -> Machine -> Either String ()
isValidTransition t machine
  | action t /= "LEFT" && action t /= "RIGHT" = Left $ "Invalid action: " ++ action t
  | to_state t `notElem` states machine = Left $ "Invalid to_state: " ++ to_state t ++ " is not in the list of states."
  | write t `notElem` alphabet machine = Left $ "Invalid write: " ++ write t ++ " should be in the alphabet."
  | read t `notElem` alphabet machine = Left $ "Invalid read: " ++ read t ++ " should be in the alphabet."
  | otherwise = Right ()

areTransitionsValid :: Transitions -> Machine -> Either String ()
areTransitionsValid machineTransitions machine =
  let errors = concatMap validateStateTransitions (Map.toList machineTransitions)
      finalStatesErrors = validateFinalStatesCoverage machineTransitions (finals machine)
   in case errors ++ finalStatesErrors of
        [] -> Right ()
        errs -> Left (unlines errs)
  where
    validateStateTransitions :: (String, [Transition]) -> [String]
    validateStateTransitions (state, ts) =
      let readChars = map read ts
          duplicates = filter (\c -> length (filter (== c) readChars) > 1) readChars
       in if not (null duplicates)
            then ["Ambiguous transitions: multiple reads of '" ++ head duplicates ++ "' in state '" ++ state ++ "'"]
            else lefts $ map (`isValidTransition` machine) ts

    validateFinalStatesCoverage :: Transitions -> [String] -> [String]
    validateFinalStatesCoverage mt finalStates =
      let toStates = concatMap (map to_state . snd) (Map.toList mt)
          missingFinals = filter (`notElem` toStates) finalStates
       in (["Final states not covered in any transition's to_state: " ++ unwords missingFinals | not (null missingFinals)])

maxSteps :: Integer
maxSteps = 10000 -- TODO user-defined (--max_steps=n)

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
      let key = (stateKey, head $ read transition)
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
      blankGood = head (blank machine),
      statesGood = states machine,
      initialGood = initial machine,
      finalsGood = finals machine,
      transitionsGood = goodifyTransitions $ transitions machine
    }

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0 ..]

createTape :: String -> Tape
createTape input = fromList $ enumerate input

-- TODO: return start when we'll try to detect loops
-- TODO: check (ChatGPT) (forgot to delete blanks at the end and start)
stringifyTape :: Tape -> Integer -> Char -> String
stringifyTape tape pos blank = "[" ++ concatMap showCell sortedCells ++ "]"
  where
    sortedCells = sort (Map.toList tape)
    filterBlanks = reverse . dropWhile (\(_, c) -> c == blank) . reverse . dropWhile (\(_, c) -> c == blank)
    showCell (k, v)
      | k == pos = "<" ++ [v] ++ ">"
      | otherwise = [v]
    nonBlankCells = filterBlanks sortedCells

-- Better functions names for loop, doJob, ftTuring

loop :: Bool -> Integer -> MachineGood -> Tape -> String -> Integer -> IO (Tape, String)
loop debug remainingSteps machineGood tape state pos
  | state `elem` finalsGood machineGood = return (tape, "Final state: " ++ state)
  | remainingSteps == 0 = return (tape, "No final state found after " ++ show maxSteps ++ " steps")
  | otherwise = do
      let blank = blankGood machineGood
      let newTape = if pos `member` tape then tape else insert pos blank tape
      let cell = fromJust $ Map.lookup pos newTape
      case Map.lookup (state, cell) $ transitionsGood machineGood of
        Nothing -> return (newTape, "Unexpected transition: (" ++ state ++ ", " ++ [cell] ++ ")")
        Just tv -> do
          when debug $ putStrLn $ stringifyTape newTape pos blank ++ " (" ++ state ++ ", " ++ [cell] ++ ") -> (" ++ toState tv ++ ", " ++ writeGood tv ++ ", " ++ (if isLeft tv then "LEFT" else "RIGHT") ++ ")"
          loop debug (remainingSteps - 1) machineGood (insert pos (head $ writeGood tv) newTape) (toState tv) (if isLeft tv then pos - 1 else pos + 1)

-- TODO: completeState stuff

doJob :: Machine -> String -> Bool -> IO ()
doJob machine input debug = do
  putStrLn "doJob"
  let machineGood = goodifyMachine machine
  putStrLn "machineGood"
  let tape = createTape input
  putStrLn "tape"
  let state = initialGood machineGood
  when debug $ putStrLn $ printMachine machine
  (finalTape, finalMessage) <- loop debug maxSteps machineGood tape state 0
  putStrLn $ stringifyTape finalTape (-42) (blankGood machineGood) ++ " " ++ finalMessage

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
