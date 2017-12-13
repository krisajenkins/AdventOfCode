module Year2017.Day13Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day13 (scannerCost, solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day13" do
    scannerCostTests
    solution1Tests
    solution2Tests

scannerCostTests :: forall eff. TestSuite eff
scannerCostTests =
  test "scannerCost" do
    equal
      24
      (sum $ map scannerCost [ 0 /\ 3
                             , 1 /\ 2
                             , 4 /\ 4
                             , 6 /\ 4
                             ])


solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 1904 answer

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal (Just 3833504) answer
