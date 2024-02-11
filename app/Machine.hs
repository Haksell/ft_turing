{-# LANGUAGE DeriveGeneric #-}

module Machine where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import GHC.Generics (Generic)

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

data Transition = Transition
  { read :: String,
    to_state :: String,
    write :: String,
    action :: String
  }
  deriving (Show, Generic)

instance FromJSON Machine

instance FromJSON Transition

parseMachine :: B.ByteString -> Either String Machine
parseMachine = eitherDecode

validateMachine :: Machine -> Either String Machine
validateMachine machine
  | initial machine `notElem` states machine = Left "Initial state is not in the list of states."
  | not (all (`elem` states machine) (finals machine)) = Left "Not all final states are in the list of states."
  | otherwise = Right machine
