module Year2017.Day10Test where

import Prelude
import Year2017.Day10 (Rope(..), knotHash, showHash, solution1, solution2, step, toLengths, twist, xorBlock)
import Control.Monad.State (evalState, execState)
import Data.Traversable (traverse)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

all :: forall eff. TestSuite eff
all =
  suite "Day10" do
    twistTests
    stepTests
    xorBlockTests
    showHashTests
    toLengthsTests
    knotHashTests
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
      (Rope { skip: 4, distanceTravelled: 19, items: [2, 4, 3, 0, 1] })
      (execState
         (traverse step [3, 4, 1, 5])
         (Rope { skip: 0, distanceTravelled: 0, items: [0, 1, 2, 3, 4] }))
    equal
      (Rope { skip: 1, distanceTravelled: 20, items: [0, 1, 2, 3, 4] })
      (execState
         (traverse step [20])
         (Rope { skip: 0, distanceTravelled: 0, items: [0, 1, 2, 3, 4] }))

solution1Tests :: forall eff. TestSuite eff
solution1Tests =
  test "Solution 1" do
    equal 23715 solution1

xorBlockTests :: forall eff. TestSuite eff
xorBlockTests =
  test "xorBlock" do
    equal 64 (xorBlock [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22])

showHashTests :: forall eff. TestSuite eff
showHashTests =
  test "showHash" do
    equal "050cc8" (showHash [5, 12, 200])
    equal "4007ff" (showHash [64, 7, 255])

toLengthsTests :: forall eff. TestSuite eff
toLengthsTests =
  test "toLengths" do
    equal [49, 44, 50, 44, 51]
      (toLengths "1,2,3")

knotHashTests :: forall eff. TestSuite eff
knotHashTests =
  test "knotHash" do
    equal "a2582a3a0e66e6e86e3812dcb672a272" (knotHash "")
    equal "33efeb34ea91902bb2f59c9920caa6cd" (knotHash "AoC 2017")
    equal "3efbe78a8d82f29979031a4aa0b16a9d" (knotHash "1,2,3")
    equal "63960835bcdc130f0b66d7ff4f6a5a8e" (knotHash "1,2,4")

solution2Tests :: forall eff. TestSuite eff
solution2Tests =
  test "Solution 2" do
    equal "541dc3180fd4b72881e39cf925a50253" solution2
