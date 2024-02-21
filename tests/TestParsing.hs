module Main where

import qualified Data.ByteString.Lazy as BL
import Machine
import qualified System.Exit as Exit
import System.FilePath ((</>))
import Test.HUnit

jsonBasePath :: FilePath
jsonBasePath = "tests" </> "resources" </> "invalid"

parsingErrorFiles :: [(String, FilePath)]
parsingErrorFiles =
  [ ("Error in $: parsing ParsedMachine failed, expected Object, but encountered Array", jsonBasePath </> "parsing_error_list.json"),
    ("Unexpected end-of-input, expecting , or }", jsonBasePath </> "parsing_error_missing_bracket.json"),
    ("Error in $: key \"states\" not found", jsonBasePath </> "parsing_error_missing_keys.json"),
    ("Error in $: key \"name\" not found", jsonBasePath </> "parsing_error_missing_keys2.json"),
    ("Error in $.name: expected String, but encountered Number", jsonBasePath </> "parsing_error_name_not_string.json"),
    ("Error in $: parsing ParsedMachine failed, expected Object, but encountered String", jsonBasePath </> "parsing_error_string.json"),
    ("Error in $: key \"blank\" not found", jsonBasePath </> "parsing_error_typo_key.json"),
    ("Error in $.transitions.scanright[0]: parsing ParsedMachine.ParsedTransition(ParsedTransition) failed, key \"to_state\" not found", jsonBasePath </> "parsing_error_transition_to_state_empty.json"),
    ("Machine name can't be empty", jsonBasePath </> "validation_error_name_empty.json"),
    ("Alphabet len should be at least 2 characters", jsonBasePath </> "validation_error_alphabet_len.json"),
    ("Error in $: Alphabet must contain single-character strings", jsonBasePath </> "validation_error_alphabet_contains_non_1_char_string.json"),
    ("Alphabet must contain unique elements", jsonBasePath </> "validation_error_alphabet_not_unique.json"),
    ("Blank should be in the alphabet", jsonBasePath </> "validation_error_blank_not_in_alphabet.json"),
    ("At least 2 states are expected (initial, finals)", jsonBasePath </> "validation_error_states_len.json"),
    ("State name can't be empty", jsonBasePath </> "validation_error_state_name_empty.json"),
    ("States names should be unique", jsonBasePath </> "validation_error_state_name_duplicates.json"),
    ("Initial state is not in the list of states.", jsonBasePath </> "validation_error_state_initial_not_in_states.json"),
    ("At least 1 finals expected.", jsonBasePath </> "validation_error_state_finals_empty.json"),
    ("Not all final states are in the list of states.", jsonBasePath </> "validation_error_state_finals_not_in_states.json"),
    ("Duplicates in finals doesn't make sense.", jsonBasePath </> "validation_error_finals_duplicates.json"),
    ("Initial value is equal to finals, program will exit immediately.", jsonBasePath </> "validation_error_finals_equal_initial.json"),
    ("Invalid to_state: donut is not in the list of states.\n", jsonBasePath </> "validation_error_transition_to_state_should_be_in_states.json"),
    ("Invalid action: DOWN\n", jsonBasePath </> "validation_error_transition_action_not_left_or_right.json"),
    ("Error in $.transitions.scanright[0].write: parsing Char failed, expected a string of length 1", jsonBasePath </> "validation_error_transition_write_not_from_alphabet.json"),
    ("Error in $.transitions.scanright[0].read: parsing Char failed, expected a string of length 1", jsonBasePath </> "validation_error_transition_read_not_from_alphabet.json"),
    ("Final states not covered in any transition's to_state: uncovered\n", jsonBasePath </> "validation_error_transition_uncovered_finals.json"),
    ("Final states not covered in any transition's to_state: HALT\n", jsonBasePath </> "validation_error_transition_one_and_uncovered_finals.json"),
    ("Ambiguous transitions: multiple reads of '..' in state 'scanright'\n", jsonBasePath </> "validation_error_transition_ambiguous_read.json"),
    ("State 'what is it?' is not in the list of states.\n", jsonBasePath </> "validation_error_transition_unknow_state.json"),
    ( unlines
        [ "Invalid to_state: cookie is not in the list of states.",
          "Invalid action: DOWN",
          "Invalid to_state: donut is not in the list of states.",
          "Invalid action: UP",
          "Invalid to_state: cake is not in the list of states.",
          "Invalid action: UPSIDE DOWN",
          "Invalid to_state: brownie is not in the list of states."
        ],
      jsonBasePath </> "validation_error_transition_mutliple_errors.json"
    )
  ]

buildMachineShouldFailWhenJsonIsIncorrect :: (String, String) -> Test
buildMachineShouldFailWhenJsonIsIncorrect (expectedErrorMsg, jsonFilePath) = TestCase $ do
  jsonFileContents <- BL.readFile jsonFilePath
  case buildMachine jsonFileContents of
    Left actualErrorMsg -> assertEqual "Error message mismatch" expectedErrorMsg actualErrorMsg
    Right _ -> assertFailure $ jsonFilePath ++ "Expected failure, but parsing succeeded"

tests :: Test
tests = TestList $ map (TestLabel "buildMachineShouldFailWhenJsonIsIncorrect" . buildMachineShouldFailWhenJsonIsIncorrect) parsingErrorFiles

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess