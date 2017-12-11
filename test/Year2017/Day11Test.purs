module Year2017.Day11Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (length)
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day11 (Direction(..), distance, readInput, solution1, solution2, walk)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day11" do
    distanceTests
    readInputTests
    solution1Tests
    solution2Tests

distanceTests :: forall eff. TestSuite eff
distanceTests =
  test "Distance" do
    equal 3 $ distance $ walk [NE, NE, NE]
    equal 0 $ distance $ walk [NE, NE, SW, SW]
    equal 2 $ distance $ walk [NE, NE, S, S]
    equal 3 $ distance $ walk [SE, SW, SE, SW, SW]

readInputTests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    answer <- liftEff readInput
    equal 8223 (length answer)

solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 722 answer

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 1551 answer
