{-# LANGUAGE DeriveGeneric #-}

module Machine where

import Data.Aeson (
    defaultOptions,
    eitherDecode,
 )
import Data.Aeson.Types (FromJSON (parseJSON), Options, genericParseJSON, rejectUnknownFields)
import qualified Data.ByteString.Lazy as B
import Data.Either
import Data.List (nub)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Prelude hiding (read)

type Transitions = Map.Map String [Transition]

data Machine = Machine
    { name :: String
    , alphabet :: [String]
    , blank :: String
    , states :: [String]
    , initial :: String
    , finals :: [String]
    , transitions :: Transitions
    }
    deriving (Show, Generic)

data Transition = Transition
    { read :: String
    , to_state :: String
    , write :: String
    , action :: String
    }
    deriving (Show, Generic)

instance FromJSON Machine where
    parseJSON = genericParseJSON $ aesonOptions "Machine"
instance FromJSON Transition where
    parseJSON = genericParseJSON $ aesonOptions "Transition"

aesonOptions :: String -> Options
aesonOptions _ = defaultOptions{rejectUnknownFields = True}

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
    validateFinalStatesCoverage transitions finalStates =
        let toStates = concatMap (map to_state . snd) (Map.toList transitions) -- Extract all 'to_state' values from transitions
            missingFinals = filter (`notElem` toStates) finalStates -- Find final states not present in 'toStates'
         in (["Final states not covered in any transition's to_state: " ++ unwords missingFinals | not (null missingFinals)])
