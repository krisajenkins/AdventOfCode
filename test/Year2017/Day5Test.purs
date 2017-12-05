module Year2017.Day5Test where

import Prelude

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Utils (inc)
import Year2017.Day5 (State(State), runCPU, sampleInput, solution1, solution2, weirdUpdateRule)

all :: forall eff. TestSuite eff
all =
  suite "Day5" do
    runCPUTests
    solution1Tests
    solution2Tests

runCPUTests :: forall eff. TestSuite eff
runCPUTests =
  test "runCPU" do
    equal
      (State { steps: 5
             , counter: 5
             , instructions: [2, 5, 0, 1, -2]
             })
      (runCPU inc sampleInput)
    equal
      (State { steps: 10
             , counter: 5
             , instructions: [2, 3, 2, 3, -1]
             })
      (runCPU weirdUpdateRule sampleInput)

solution1Tests :: forall eff. TestSuite eff
solution1Tests =
  test "Solution 1" $
    equal 359348 solution1

solution2Tests :: forall eff. TestSuite eff
solution2Tests =
  test "Solution 2" $
    equal 27688760 solution2
