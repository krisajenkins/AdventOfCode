module Year2017.Day24Test where

import Prelude
import Year2017.Day24 (readInput, solution1, solution2)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.List as List
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day24" do
    readInputTests
    solution1Tests
    solution2Tests

readInputTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    answer <- liftEff readInput
    equal 55 (List.length answer)

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 1859 answer

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 1799 answer
