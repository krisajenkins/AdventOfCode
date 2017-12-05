module Year2017.Day5Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.ST (ST)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Utils (inc)
import Year2017.Day5 (State(State), runCPU, sampleInput, solution1, solution2, weirdUpdateRule)

all :: forall h eff. TestSuite (st :: ST h | eff)
all =
  suite "Day5" do
    runCPUTests
    solution1Tests
    solution2Tests

runCPUTests :: forall h eff. TestSuite (st :: ST h | eff)
runCPUTests =
  test "runCPU" do
    result1 <- liftEff $ runCPU inc sampleInput
    equal
      (State { steps: 5
             , programCounter: 5
             , instructions: [2, 5, 0, 1, -2]
             })
      result1
    result2 <- liftEff $ runCPU weirdUpdateRule sampleInput
    equal
      (State { steps: 10
             , programCounter: 5
             , instructions: [2, 3, 2, 3, -1]
             })
      result2

solution1Tests :: forall h eff. TestSuite (st :: ST h | eff)
solution1Tests =
  test "Solution 1" do
    result <- liftEff solution1
    equal 359348 result

solution2Tests :: forall h eff. TestSuite (st :: ST h | eff)
solution2Tests =
  test "Solution 2" do
    result <- liftEff solution2
    equal 27688760 result
