module Main where

import qualified Data.ByteString.Lazy as BL
import Machine
import qualified System.Exit as Exit
import System.FilePath ((</>))
import Test.HUnit

jsonBasePath :: FilePath
jsonBasePath = "tests" </> "resources" </> "json"

testFiles :: [(String, FilePath)]
testFiles =
  [ ("Error in $: parsing Machine.Machine(Machine) failed, expected Object, but encountered Array", jsonBasePath </> "parsing_error_list.json"),
    ("Unexpected end-of-input, expecting , or }", jsonBasePath </> "parsing_error_missing_bracket.json"),
    ("Error in $: parsing Machine.Machine(Machine) failed, key \"states\" not found", jsonBasePath </> "parsing_error_missing_keys.json"),
    ("Error in $.name: expected String, but encountered Number", jsonBasePath </> "parsing_error_name_not_string.json"),
    ("Error in $: parsing Machine.Machine(Machine) failed, expected Object, but encountered String", jsonBasePath </> "parsing_error_string.json"),
    ("Error in $: parsing Machine.Machine(Machine) failed, key \"blank\" not found", jsonBasePath </> "parsing_error_typo_key.json"),
    ("Stub", jsonBasePath </> "parsing_error_unexpected_keys.json")
  ]

parseMachine_should_fails_when_json_is_incorrect :: (String, String) -> Test
parseMachine_should_fails_when_json_is_incorrect (expectedErrorMsg, jsonFilePath) = TestCase $ do
  jsonFileContents <- BL.readFile jsonFilePath
  case parseMachine jsonFileContents of
    Left actualErrorMsg -> assertEqual "Error message mismatch" expectedErrorMsg actualErrorMsg
    Right _ -> assertFailure "Expected failure, but parsing succeeded"

tests :: Test
tests = TestList $ map (TestLabel "parseMachine_should_fails_when_json_is_incorrect" . parseMachine_should_fails_when_json_is_incorrect) testFiles

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess