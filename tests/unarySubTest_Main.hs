module UnarySubTest where

import qualified Data.ByteString.Lazy as BL
import qualified System.Exit as Exit
import System.FilePath ((</>))
import Test.HUnit

type Expected = String
type Input = String

jsonBasePath :: FilePath
jsonBasePath = "tests" </> "resources" </> "valid"

testTarget :: FilePath
testTarget = jsonBasePath </> "unary_sub.json"

testCases :: [(Input, Expected)]
testCases = [("input", "expected")]

executeMachineTest :: (Input, Expected) -> Test
executeMachineTest (input, expected) = TestCase $ do
  (finalTape, finalPos, finalMessage) <- execute debug maxSteps maxSteps machine (createTape input) (initial machine) 0
  assertEqual "Error message mismatch" "Error" expected

tests :: Test
tests = TestList $ map (TestLabel "executeMachineTest" . executeMachineTest) testCases

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess