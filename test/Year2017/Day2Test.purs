module Year2017.Day2Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day2 (solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day2" do
    solution1Tests
    solution2Tests

solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    result <- liftEff solution1
    equal (Right 36174) result

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    result <- liftEff solution2
    equal (Right 244) result
