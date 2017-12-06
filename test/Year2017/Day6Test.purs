module Year2017.Day6Test where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.ST (ST)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Year2017.Day6 (offsetList, redistribute, solve, solution1, solution2)
import Year2017.Day6 as Day6

all :: forall eff. TestSuite eff
all =
  suite "Day6" do
    offsetListTests
    redistributeTests
    solveTests
    solution1Tests
    solution2Tests

offsetListTests :: forall eff. TestSuite eff
offsetListTests =
  test "Offset lists" do
    equal
      [-10, 3, 3, 2, 2]
      (offsetList { items: 12, length: 5, index: 0 })
    equal
      [2, -10, 3, 3, 2]
      (offsetList { items: 12, length: 5, index: 1 })
    equal
      [2, 2, -6, 2]
      (offsetList { items: 7, length: 4, index: 2 })

redistributeTests :: forall eff. TestSuite eff
redistributeTests =
  test "Redistribute" do
    equal
      [2, 4, 1, 2]
      (redistribute [0, 2, 7, 0])
    equal
      [3, 1, 2, 3]
      (redistribute [2, 4, 1, 2])

solveTests :: forall eff. TestSuite eff
solveTests =
  test "Solve" do
    let ({steps} /\ cycleAfter) = solve [0, 2, 7, 0]
    equal 5 steps
    equal 4 cycleAfter

solution1Tests :: forall eff. TestSuite eff
solution1Tests =
  test "Solution 1" do
    equal 4074 solution1

solution2Tests :: forall eff. TestSuite eff
solution2Tests =
  test "Solution 2" do
    equal 2793 solution2
