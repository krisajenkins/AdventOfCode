module Year2017.Day1Test where

import Prelude

import Test.Unit (TestSuite, test)
import Test.Unit.Assert (equal)
import Year2017.Day1 (solution1, solution2)

all :: forall eff. TestSuite eff
all =
  test "Day1" do
    equal 1141 solution1
    equal 950 solution2
