module Year2017.Day7Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day7 (Summary(Summary), readInput, solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day7" do
    parserTests
    solution1Tests
    solution2Tests

parserTests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
parserTests =
  test "Parser" do
    graph <- liftEff readInput
    equal
      (Just (100 /\ ["kyspusd", "iipyba"]))
      (Map.lookup "fscnkbm" graph)

solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal
      (Set.singleton "cyrupz")
      answer

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal
      [(Left [ Summary {myWeight: 1023, parent: "slzaeep", total: 1123}
             , Summary {myWeight: 877, parent: "hiotqxu", total: 1123}
             , Summary {myWeight: 171, parent: "qppggd", total: 1123}
             , Summary {myWeight: 397, parent: "iahug", total: 1123}
             , Summary {myWeight: 201, parent: "cwwwj", total: 1131}
             , Summary {myWeight: 27, parent: "upfhsu", total: 1123}
             , Summary {myWeight: 46, parent: "jjlodie", total: 1123}
             ])]
      answer
