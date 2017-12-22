module Year2017.Day22Test where

import Prelude

import Year2017.Day22
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
  suite "Day22" do
    solution1Tests
    solution2Tests

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 5462 answer

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 2512135 answer
