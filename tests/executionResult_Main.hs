{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Executor (createTape, execute, stringifyTape)
import Machine (Machine (blank), buildMachine, initial)
import System.Exit qualified as Exit
import System.FilePath ((</>))
import Test.HUnit

type ExpectedTape = String
type ExpectedMsg = String
type Input = String

validJsonBasePath :: FilePath
validJsonBasePath = "tests" </> "resources" </> "valid"

ambiguousJsonBasePath :: FilePath
ambiguousJsonBasePath = "tests" </> "resources" </> "ambiguous"

binaryNotTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
binaryNotTestCases =
  [ ("", "[]", "Final state: HALT")
  , ("0", "[1]", "Final state: HALT")
  , ("1", "[0]", "Final state: HALT")
  , ("00", "[11]", "Final state: HALT")
  , ("01", "[10]", "Final state: HALT")
  , ("10", "[01]", "Final state: HALT")
  , ("11", "[00]", "Final state: HALT")
  , ("000", "[111]", "Final state: HALT")
  , ("001", "[110]", "Final state: HALT")
  , ("010", "[101]", "Final state: HALT")
  , ("011", "[100]", "Final state: HALT")
  , ("100", "[011]", "Final state: HALT")
  , ("101", "[010]", "Final state: HALT")
  , ("110", "[001]", "Final state: HALT")
  , ("111", "[000]", "Final state: HALT")
  , ("11111111111111111111111111111111", "[00000000000000000000000000000000]", "Final state: HALT")
  ]

t0to2nTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
t0to2nTestCases =
  [ ("", "[y]", "Final state: HALT")
  , ("0", "[0n]", "Final state: HALT")
  , ("00", "[00y]", "Final state: HALT")
  , ("000", "[000n]", "Final state: HALT")
  , ("0000", "[0000y]", "Final state: HALT")
  , ("00000", "[00000n]", "Final state: HALT")
  , ("000000", "[000000y]", "Final state: HALT")
  , ("0000000", "[0000000n]", "Final state: HALT")
  , ("00000000", "[00000000y]", "Final state: HALT")
  , ("000000000", "[000000000n]", "Final state: HALT")
  ]

t1ton1tonTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
t1ton1tonTestCases =
  [ ("", "[y]", "Final state: HALT")
  , ("0", "[0n]", "Final state: HALT")
  , ("1", "[1n]", "Final state: HALT")
  , ("00", "[00n]", "Final state: HALT")
  , ("01", "[01y]", "Final state: HALT")
  , ("10", "[10n]", "Final state: HALT")
  , ("11", "[11n]", "Final state: HALT")
  , ("000", "[000n]", "Final state: HALT")
  , ("001", "[001n]", "Final state: HALT")
  , ("010", "[010n]", "Final state: HALT")
  , ("011", "[011n]", "Final state: HALT")
  , ("100", "[100n]", "Final state: HALT")
  , ("101", "[101n]", "Final state: HALT")
  , ("110", "[110n]", "Final state: HALT")
  , ("111", "[111n]", "Final state: HALT")
  , ("0000", "[0000n]", "Final state: HALT")
  , ("0001", "[0001n]", "Final state: HALT")
  , ("0010", "[0010n]", "Final state: HALT")
  , ("0011", "[0011y]", "Final state: HALT")
  , ("0100", "[0100n]", "Final state: HALT")
  , ("0101", "[0101n]", "Final state: HALT")
  , ("0110", "[0110n]", "Final state: HALT")
  , ("0111", "[0111n]", "Final state: HALT")
  , ("1000", "[1000n]", "Final state: HALT")
  , ("1001", "[1001n]", "Final state: HALT")
  , ("1010", "[1010n]", "Final state: HALT")
  , ("1011", "[1011n]", "Final state: HALT")
  , ("1100", "[1100n]", "Final state: HALT")
  , ("1101", "[1101n]", "Final state: HALT")
  , ("1110", "[1110n]", "Final state: HALT")
  , ("1111", "[1111n]", "Final state: HALT")
  , ("00000", "[00000n]", "Final state: HALT")
  , ("00001", "[00001n]", "Final state: HALT")
  , ("00010", "[00010n]", "Final state: HALT")
  , ("00011", "[00011n]", "Final state: HALT")
  , ("00100", "[00100n]", "Final state: HALT")
  , ("00101", "[00101n]", "Final state: HALT")
  , ("00110", "[00110n]", "Final state: HALT")
  , ("00111", "[00111n]", "Final state: HALT")
  , ("01000", "[01000n]", "Final state: HALT")
  , ("01001", "[01001n]", "Final state: HALT")
  , ("01010", "[01010n]", "Final state: HALT")
  , ("01011", "[01011n]", "Final state: HALT")
  , ("01100", "[01100n]", "Final state: HALT")
  , ("01101", "[01101n]", "Final state: HALT")
  , ("01110", "[01110n]", "Final state: HALT")
  , ("01111", "[01111n]", "Final state: HALT")
  , ("10000", "[10000n]", "Final state: HALT")
  , ("10001", "[10001n]", "Final state: HALT")
  , ("10010", "[10010n]", "Final state: HALT")
  , ("10011", "[10011n]", "Final state: HALT")
  , ("10100", "[10100n]", "Final state: HALT")
  , ("10101", "[10101n]", "Final state: HALT")
  , ("10110", "[10110n]", "Final state: HALT")
  , ("10111", "[10111n]", "Final state: HALT")
  , ("11000", "[11000n]", "Final state: HALT")
  , ("11001", "[11001n]", "Final state: HALT")
  , ("11010", "[11010n]", "Final state: HALT")
  , ("11011", "[11011n]", "Final state: HALT")
  , ("11100", "[11100n]", "Final state: HALT")
  , ("11101", "[11101n]", "Final state: HALT")
  , ("11110", "[11110n]", "Final state: HALT")
  , ("11111", "[11111n]", "Final state: HALT")
  , ("000000", "[000000n]", "Final state: HALT")
  , ("000001", "[000001n]", "Final state: HALT")
  , ("000010", "[000010n]", "Final state: HALT")
  , ("000011", "[000011n]", "Final state: HALT")
  , ("000100", "[000100n]", "Final state: HALT")
  , ("000101", "[000101n]", "Final state: HALT")
  , ("000110", "[000110n]", "Final state: HALT")
  , ("000111", "[000111y]", "Final state: HALT")
  , ("001000", "[001000n]", "Final state: HALT")
  , ("001001", "[001001n]", "Final state: HALT")
  , ("001010", "[001010n]", "Final state: HALT")
  , ("001011", "[001011n]", "Final state: HALT")
  , ("001100", "[001100n]", "Final state: HALT")
  , ("001101", "[001101n]", "Final state: HALT")
  , ("001110", "[001110n]", "Final state: HALT")
  , ("001111", "[001111n]", "Final state: HALT")
  , ("010000", "[010000n]", "Final state: HALT")
  , ("010001", "[010001n]", "Final state: HALT")
  , ("010010", "[010010n]", "Final state: HALT")
  , ("010011", "[010011n]", "Final state: HALT")
  , ("010100", "[010100n]", "Final state: HALT")
  , ("010101", "[010101n]", "Final state: HALT")
  , ("010110", "[010110n]", "Final state: HALT")
  , ("010111", "[010111n]", "Final state: HALT")
  , ("011000", "[011000n]", "Final state: HALT")
  , ("011001", "[011001n]", "Final state: HALT")
  , ("011010", "[011010n]", "Final state: HALT")
  , ("011011", "[011011n]", "Final state: HALT")
  , ("011100", "[011100n]", "Final state: HALT")
  , ("011101", "[011101n]", "Final state: HALT")
  , ("011110", "[011110n]", "Final state: HALT")
  , ("011111", "[011111n]", "Final state: HALT")
  , ("100000", "[100000n]", "Final state: HALT")
  , ("100001", "[100001n]", "Final state: HALT")
  , ("100010", "[100010n]", "Final state: HALT")
  , ("100011", "[100011n]", "Final state: HALT")
  , ("100100", "[100100n]", "Final state: HALT")
  , ("100101", "[100101n]", "Final state: HALT")
  , ("100110", "[100110n]", "Final state: HALT")
  , ("100111", "[100111n]", "Final state: HALT")
  , ("101000", "[101000n]", "Final state: HALT")
  , ("101001", "[101001n]", "Final state: HALT")
  , ("101010", "[101010n]", "Final state: HALT")
  , ("101011", "[101011n]", "Final state: HALT")
  , ("101100", "[101100n]", "Final state: HALT")
  , ("101101", "[101101n]", "Final state: HALT")
  , ("101110", "[101110n]", "Final state: HALT")
  , ("101111", "[101111n]", "Final state: HALT")
  , ("110000", "[110000n]", "Final state: HALT")
  , ("110001", "[110001n]", "Final state: HALT")
  , ("110010", "[110010n]", "Final state: HALT")
  , ("110011", "[110011n]", "Final state: HALT")
  , ("110100", "[110100n]", "Final state: HALT")
  , ("110101", "[110101n]", "Final state: HALT")
  , ("110110", "[110110n]", "Final state: HALT")
  , ("110111", "[110111n]", "Final state: HALT")
  , ("111000", "[111000n]", "Final state: HALT")
  , ("111001", "[111001n]", "Final state: HALT")
  , ("111010", "[111010n]", "Final state: HALT")
  , ("111011", "[111011n]", "Final state: HALT")
  , ("111100", "[111100n]", "Final state: HALT")
  , ("111101", "[111101n]", "Final state: HALT")
  , ("111110", "[111110n]", "Final state: HALT")
  , ("111111", "[111111n]", "Final state: HALT")
  , ("00001111", "[00001111y]", "Final state: HALT")
  , ("0000011111", "[0000011111y]", "Final state: HALT")
  , ("000000111111", "[000000111111y]", "Final state: HALT")
  , ("00000001111111", "[00000001111111y]", "Final state: HALT")
  ]

palindromeTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
palindromeTestCases =
  [ ("", "[y]", "Final state: HALT")
  , ("0", "[0y]", "Final state: HALT")
  , ("1", "[1y]", "Final state: HALT")
  , ("00", "[00y]", "Final state: HALT")
  , ("01", "[01n]", "Final state: HALT")
  , ("10", "[10n]", "Final state: HALT")
  , ("11", "[11y]", "Final state: HALT")
  , ("000", "[000y]", "Final state: HALT")
  , ("001", "[001n]", "Final state: HALT")
  , ("010", "[010y]", "Final state: HALT")
  , ("011", "[011n]", "Final state: HALT")
  , ("100", "[100n]", "Final state: HALT")
  , ("101", "[101y]", "Final state: HALT")
  , ("110", "[110n]", "Final state: HALT")
  , ("111", "[111y]", "Final state: HALT")
  , ("0000", "[0000y]", "Final state: HALT")
  , ("0001", "[0001n]", "Final state: HALT")
  , ("0010", "[0010n]", "Final state: HALT")
  , ("0011", "[0011n]", "Final state: HALT")
  , ("0100", "[0100n]", "Final state: HALT")
  , ("0101", "[0101n]", "Final state: HALT")
  , ("0110", "[0110y]", "Final state: HALT")
  , ("0111", "[0111n]", "Final state: HALT")
  , ("1000", "[1000n]", "Final state: HALT")
  , ("1001", "[1001y]", "Final state: HALT")
  , ("1010", "[1010n]", "Final state: HALT")
  , ("1011", "[1011n]", "Final state: HALT")
  , ("1100", "[1100n]", "Final state: HALT")
  , ("1101", "[1101n]", "Final state: HALT")
  , ("1110", "[1110n]", "Final state: HALT")
  , ("1111", "[1111y]", "Final state: HALT")
  , ("00000", "[00000y]", "Final state: HALT")
  , ("00001", "[00001n]", "Final state: HALT")
  , ("00010", "[00010n]", "Final state: HALT")
  , ("00011", "[00011n]", "Final state: HALT")
  , ("00100", "[00100y]", "Final state: HALT")
  , ("00101", "[00101n]", "Final state: HALT")
  , ("00110", "[00110n]", "Final state: HALT")
  , ("00111", "[00111n]", "Final state: HALT")
  , ("01000", "[01000n]", "Final state: HALT")
  , ("01001", "[01001n]", "Final state: HALT")
  , ("01010", "[01010y]", "Final state: HALT")
  , ("01011", "[01011n]", "Final state: HALT")
  , ("01100", "[01100n]", "Final state: HALT")
  , ("01101", "[01101n]", "Final state: HALT")
  , ("01110", "[01110y]", "Final state: HALT")
  , ("01111", "[01111n]", "Final state: HALT")
  , ("10000", "[10000n]", "Final state: HALT")
  , ("10001", "[10001y]", "Final state: HALT")
  , ("10010", "[10010n]", "Final state: HALT")
  , ("10011", "[10011n]", "Final state: HALT")
  , ("10100", "[10100n]", "Final state: HALT")
  , ("10101", "[10101y]", "Final state: HALT")
  , ("10110", "[10110n]", "Final state: HALT")
  , ("10111", "[10111n]", "Final state: HALT")
  , ("11000", "[11000n]", "Final state: HALT")
  , ("11001", "[11001n]", "Final state: HALT")
  , ("11010", "[11010n]", "Final state: HALT")
  , ("11011", "[11011y]", "Final state: HALT")
  , ("11100", "[11100n]", "Final state: HALT")
  , ("11101", "[11101n]", "Final state: HALT")
  , ("11110", "[11110n]", "Final state: HALT")
  , ("11111", "[11111y]", "Final state: HALT")
  ]

unaryAddTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
unaryAddTestCases =
  [ ("+=", "[]", "Final state: HALT")
  , ("+1=", "[1]", "Final state: HALT")
  , ("+11=", "[11]", "Final state: HALT")
  , ("+111=", "[111]", "Final state: HALT")
  , ("+1111=", "[1111]", "Final state: HALT")
  , ("1+=", "[1]", "Final state: HALT")
  , ("1+1=", "[11]", "Final state: HALT")
  , ("1+11=", "[111]", "Final state: HALT")
  , ("1+111=", "[1111]", "Final state: HALT")
  , ("1+1111=", "[11111]", "Final state: HALT")
  , ("11+=", "[11]", "Final state: HALT")
  , ("11+1=", "[111]", "Final state: HALT")
  , ("11+11=", "[1111]", "Final state: HALT")
  , ("11+111=", "[11111]", "Final state: HALT")
  , ("11+1111=", "[111111]", "Final state: HALT")
  , ("111+=", "[111]", "Final state: HALT")
  , ("111+1=", "[1111]", "Final state: HALT")
  , ("111+11=", "[11111]", "Final state: HALT")
  , ("111+111=", "[111111]", "Final state: HALT")
  , ("111+1111=", "[1111111]", "Final state: HALT")
  , ("1111+=", "[1111]", "Final state: HALT")
  , ("1111+1=", "[11111]", "Final state: HALT")
  , ("1111+11=", "[111111]", "Final state: HALT")
  , ("1111+111=", "[1111111]", "Final state: HALT")
  , ("1111+1111=", "[11111111]", "Final state: HALT")
  ]

unaryMulTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
unaryMulTestCases =
  [ ("*=", "[*=]", "Final state: HALT")
  , ("*1=", "[*1=]", "Final state: HALT")
  , ("*11=", "[*11=]", "Final state: HALT")
  , ("*111=", "[*111=]", "Final state: HALT")
  , ("*1111=", "[*1111=]", "Final state: HALT")
  , ("1*=", "[1*=]", "Final state: HALT")
  , ("1*1=", "[1*1=1]", "Final state: HALT")
  , ("1*11=", "[1*11=11]", "Final state: HALT")
  , ("1*111=", "[1*111=111]", "Final state: HALT")
  , ("1*1111=", "[1*1111=1111]", "Final state: HALT")
  , ("11*=", "[11*=]", "Final state: HALT")
  , ("11*1=", "[11*1=11]", "Final state: HALT")
  , ("11*11=", "[11*11=1111]", "Final state: HALT")
  , ("11*111=", "[11*111=111111]", "Final state: HALT")
  , ("11*1111=", "[11*1111=11111111]", "Final state: HALT")
  , ("111*=", "[111*=]", "Final state: HALT")
  , ("111*1=", "[111*1=111]", "Final state: HALT")
  , ("111*11=", "[111*11=111111]", "Final state: HALT")
  , ("111*111=", "[111*111=111111111]", "Final state: HALT")
  , ("111*1111=", "[111*1111=111111111111]", "Final state: HALT")
  , ("1111*=", "[1111*=]", "Final state: HALT")
  , ("1111*1=", "[1111*1=1111]", "Final state: HALT")
  , ("1111*11=", "[1111*11=11111111]", "Final state: HALT")
  , ("1111*111=", "[1111*111=111111111111]", "Final state: HALT")
  , ("1111*1111=", "[1111*1111=1111111111111111]", "Final state: HALT")
  ]

brokenTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
brokenTestCases =
  [ ("l!", "[l!]", "Unexpected transition: (loop1, .)")
  , ("rrr", "[rrr]", "Unexpected transition: (loop0, .)")
  ]

infiniteTestCases :: [(Input, ExpectedTape, ExpectedMsg)]
infiniteTestCases =
  [ ("rl!", "[<r>l!]", "Infinite loop detected at state: loop0")
  , ("rrrrrlrlrll!lrl", "[rrrr<r>lrlrll!lrl]", "Infinite loop detected at state: loop1")
  ]

executeMachineTest :: String -> (Input, ExpectedTape, ExpectedMsg) -> Test
executeMachineTest fileName (input, expectedTape, expectedMsg) = TestCase $ do
  jsonContent <- BL.readFile fileName
  let machineResult = buildMachine jsonContent
  case machineResult of
    Left errorMsg -> assertFailure errorMsg
    Right machine -> do
      (finalTape, finalPos, finalMessage, _) <- execute False 10000 10000 machine (createTape input) (initial machine) 0 Set.empty
      let tapeString = stringifyTape finalTape finalPos $ blank machine
      assertEqual "Error final msg!" expectedMsg finalMessage
      assertEqual "Error final tape!" expectedTape tapeString

tests :: Test
tests =
  TestList $
    (map (TestLabel "binary_not" . executeMachineTest (validJsonBasePath </> "binary_not.json")) binaryNotTestCases)
      ++ (map (TestLabel "0to2n" . executeMachineTest (validJsonBasePath </> "0to2n.json")) t0to2nTestCases)
      ++ (map (TestLabel "0ton1ton" . executeMachineTest (validJsonBasePath </> "0ton1ton.json")) t1ton1tonTestCases)
      ++ (map (TestLabel "palindrome" . executeMachineTest (validJsonBasePath </> "palindrome.json")) palindromeTestCases)
      ++ (map (TestLabel "unary_add" . executeMachineTest (validJsonBasePath </> "unary_add.json")) unaryAddTestCases)
      ++ (map (TestLabel "unary_mul" . executeMachineTest (validJsonBasePath </> "unary_mul.json")) unaryMulTestCases)
      ++ (map (TestLabel "broken" . executeMachineTest (ambiguousJsonBasePath </> "arrows.json")) brokenTestCases)
      ++ (map (TestLabel "broken" . executeMachineTest (ambiguousJsonBasePath </> "arrows.json")) infiniteTestCases)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess