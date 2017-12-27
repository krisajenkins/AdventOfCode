module Year2017.Day25Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Lens (view)
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Year2017.Day25 (_maxSteps, readInput, solution1)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day25" do
    readInputTests
    solution1Tests

readInputTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    spec <- liftEff readInput
    equal 12523873 (view _maxSteps spec)

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 4225 answer
