module Year2017.Day12Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(Just))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day12 ( solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day12" do
    solution1Tests
    solution2Tests

solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 145 answer

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal (Just 207) answer
