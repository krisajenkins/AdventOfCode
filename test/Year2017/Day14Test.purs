module Year2017.Day14Test where

import Prelude

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Year2017.Day14 (hexStringToBits, solution1, solution2)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day14" do
    hexStringToBitsTests
    solution1Tests
    solution2Tests

hexStringToBitsTests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
hexStringToBitsTests =
  test "HexStringToBits" do
    equal (Just [true, false, true, false])
      (hexStringToBits "a")

solution1Tests :: forall eff. TestSuite eff
solution1Tests =
  test "Solution 1" do
    equal (Just 8214) solution1

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    equal (Just 1093) solution2
