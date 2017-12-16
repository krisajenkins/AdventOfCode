module Year2017.Day17Test where

import Prelude
import Year2017.Day17 (solution1, solution2, spinlock)
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

all :: forall eff. TestSuite eff
all =
  suite "Day17" do
    spinlockTests
    solution1Tests
    solution2Tests

spinlockTests :: forall eff. TestSuite eff
spinlockTests =
  test "Spinlock" do
    let answer = spinlock
                   { stepSize: 3
                   , limit: 5
                   }
    equal (Just [ 0, 5, 2, 4, 3, 1 ])
      answer.cells
    let answer2 = spinlock
                   { stepSize: 3
                   , limit: 30
                   }
    pure unit

solution1Tests :: forall eff. TestSuite eff
solution1Tests =
  test "Solution 1" do
    equal (Just 640) solution1

solution2Tests :: forall eff. TestSuite eff
solution2Tests =
  test "Solution 2" do
    equal 47949463 solution2
