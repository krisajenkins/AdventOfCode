module Year2017.Day10Test where

import Prelude
import Year2017.Day10

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State (evalState, execState)
import Data.Array as Array
import Data.Traversable (traverse)
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

all :: forall eff. TestSuite eff
all =
  suite "Day10" do
    twistTests
    stepTests
    solution1Tests
    solution2Tests

twistTests :: forall eff. TestSuite eff
twistTests =
  test "Twist" do
    equal [3, 4, 2, 1, 0]
      (twist 3 [0, 1, 2, 3, 4])

stepTests :: forall eff. TestSuite eff
stepTests =
  test "Step" do
    equal
      [ 3
      , 3 + 4 + 1
      , 3 + 4 + 1 + 1 + 2
      , 3 + 4 + 1 + 1 + 2 + 5 + 3
      ]
      (evalState
         (traverse step [3, 4, 1, 5])
         (Rope { skip: 0, distanceTravelled: 0, items: [0, 1, 2, 3, 4] }))
    equal
      (Rope { skip: 4, distanceTravelled: 19, items: [0, 3, 4, 2, 1] })
      (execState
         (traverse step [3, 4, 1, 5])
         (Rope { skip: 0, distanceTravelled: 0, items: [0, 1, 2, 3, 4] }))

solution1Tests :: forall eff. TestSuite eff
solution1Tests =
  test "Solution 1" do
    equal 23715 solution1


knotHashTests :: forall eff. TestSuite eff
knotHashTests =
  test "Knot Hash" do
    equal "a2582a3a0e66e6e86e3812dcb672a272" (knotHash "")
    equal "33efeb34ea91902bb2f59c9920caa6cd" (knotHash "AoC 2017")
    equal "3efbe78a8d82f29979031a4aa0b16a9d" (knotHash "1,2,3")
    equal "63960835bcdc130f0b66d7ff4f6a5a8e" (knotHash "1,2,4")

solution2Tests :: forall eff. TestSuite eff
solution2Tests =
  test "Solution 2" do
    equal "" solution2
