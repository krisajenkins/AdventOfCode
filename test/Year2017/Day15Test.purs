
module Year2017.Day15Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.BigInt (fromInt)
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import TestUtils (timeEff)
import Year2017.Day15 (runGenerators, solution1, solution2, strategy1)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day15" do
    runGeneratorsTests
    solution1Tests
    solution2Tests

runGeneratorsTests :: forall eff. TestSuite ( testOutput :: TESTOUTPUT, now :: NOW | eff)
runGeneratorsTests =
  test "runGenerators" do
    let answer1 = runGenerators strategy1 65 8921 5
    equal (fromInt 1352636452) answer1.a
    equal (fromInt 285222916) answer1.b
    equal 1 answer1.matchCount

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff $ timeEff (pure <<< solution1) (40 * 1000 * 1000)
    equal 573 answer.matchCount

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff $ timeEff (pure <<< solution2) (5 * 1000 * 1000)
    equal 294 answer.matchCount
