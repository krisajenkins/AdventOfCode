module Year2017.Day11Test where

import Prelude

import Year2017.Day11 (Direction(..), distance, solution1, solution2, walk)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day11" do
    distanceTests
    solution1Tests
    solution2Tests

distanceTests :: forall eff. TestSuite eff
distanceTests =
  test "Distance" do
    equal 3 $ distance $ walk [NE, NE, NE]
    equal 0 $ distance $ walk [NE, NE, SW, SW]
    equal 2 $ distance $ walk [NE, NE, S, S]
    equal 3 $ distance $ walk [SE, SW, SE, SW, SW]

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
