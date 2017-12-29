module Year2017.Day18Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.BigInt (fromInt)
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Year2017.Day18 (Instruction(..), readInput, runCPU, runDualCore, solution1, solution2)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day18" do
    readInputTests
    runCPUTests
    solution1Tests
    runDualCoreTests
    solution2Tests

readInputTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    answer <- liftEff readInput
    equal 41  (List.length answer)

runCPUTests :: forall eff. TestSuite eff
runCPUTests =
  suite "runCPU" do
    test "send/receive" do
      equal (Just (fromInt 2))
	    (runCPU [ Set 'a' (Left (fromInt 2))
                , Send (Right 'a')
                , Receive 'a'
                ])
      equal Nothing
	    (runCPU [ Set 'a' (Left (fromInt 2))
                , Send (Right 'a')
                , Receive 'b'
                ])
    test "remainder" do
      equal Nothing
	    (runCPU [ Set 'a' (Left (fromInt 2))
                , Remainder 'a' (Left (fromInt 2))
                , Send (Right 'a')
                , Receive 'a'
                ])
      equal (Just (fromInt 1))
	    (runCPU [ Set 'a' (Left (fromInt 3))
                , Remainder 'a' (Left (fromInt 2))
                , Send (Right 'a')
                , Receive 'a'
                ])
    test "multiply" do
      equal (Just (fromInt 8))
	    (runCPU [ Set 'a' (Left (fromInt 2))
                , Set 'b' (Left (fromInt 4))
                , Multiply 'a' (Right 'b')
                , Send (Right 'a')
                , Receive 'a'
                ])
      equal Nothing
	    (runCPU [ Set 'a' (Left (fromInt 2))
                , Multiply 'a' (Right 'b')
                , Send (Right 'a')
                , Receive 'a'
                ])
    test "add" do
      equal (Just (fromInt 3))
	    (runCPU [ Add 'b' (Left (fromInt 3))
                , Send (Right 'b')
                , Receive 'b'
                ])
      equal (Just (fromInt 2))
	    (runCPU [ Set 'a' (Left (fromInt 2))
                , Add 'b' (Right 'a')
                , Send (Right 'b')
                , Receive 'b'
                ])
      equal (Just (fromInt 5))
	    (runCPU [ Set 'b' (Left (fromInt 5))
                , Add 'b' (Right 'a')
                , Send (Right 'b')
                , Receive 'b'
                ])
    test "example" do
      equal (Just (fromInt 4))
	    (runCPU [ Set 'a' (Left (fromInt 1))
                , Add 'a' (Left (fromInt 2))
                , Multiply 'a' (Right 'a')
                , Remainder 'a' (Left (fromInt 5))
                , Send (Right 'a')
                , Set 'a' (Left (fromInt 0))
                , Receive 'a'
                , Jump (Right 'a') (Left (fromInt (-1)))
                , Set 'a' (Left (fromInt 1))
                , Jump (Right 'a') (Left (fromInt (-2)))
                ])

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal (Just (fromInt 2951)) answer

runDualCoreTests :: forall eff. TestSuite eff
runDualCoreTests =
  suite "runDualCore" do
    test "example" do
      equal (fromInt 3 /\ fromInt 3)
        (runDualCore [ Send (Left (fromInt 1))
                     , Send (Left (fromInt 2))
                     , Send (Right 'p')
                     , Receive 'a'
                     , Receive 'b'
                     , Receive 'c'
                     , Receive 'd'
                     ])
      equal (fromInt 3 /\ fromInt 3)
        (runDualCore [ Send (Left (fromInt 1))
                     , Send (Left (fromInt 2))
                     , Receive 'a'
                     , Receive 'b'
                     , Multiply 'a' (Right 'b')
                     , Send (Right 'b')
                     , Receive 'c'
                     ])

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal (fromInt 7493 /\ fromInt 7366) answer
