module Year2017.Day23Test where

import Prelude
import Year2017.Day23 (_multiplyCounter, _registers, readInput, solution1, solution2)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.BigInt (fromInt)
import Data.Lens (view)
import Data.Lens.At (at)
import Data.List as List
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day23" do
    readInputTests
    solution1Tests
    solution2Tests

readInputTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    answer <- liftEff $ readInput "src/Year2017/Day23.txt"
    equal 32 (List.length answer)

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal (fromInt 6724)
      (view _multiplyCounter answer)

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, console :: CONSOLE, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal (Just (fromInt 903))
      (view (_registers <<< at 'h') answer)
