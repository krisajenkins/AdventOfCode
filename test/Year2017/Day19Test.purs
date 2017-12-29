module Year2017.Day19Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Map as Map
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Year2017.Day19 (readInput, solution1, solution2)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day19" do
    readInputTests
    solution1Tests
    solution2Tests

readInputTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    world <- liftEff readInput
    equal 16689 (Map.size world)
    equal (Just '+')
      (Map.lookup (4 /\ 8) world)

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal "VEBTPXCHLI" answer

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 18702 answer
