module Year2017.Day12Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Set as Set
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day12 (connectedGroups, solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day12" do
    connectedGroupsTests
    solution1Tests
    solution2Tests

connectedGroupsTests :: forall eff. TestSuite eff
connectedGroupsTests =
  test "connectedGroups" do
    equal ([ 0 /\ 1
           , 1 /\ 2
           , 2 /\ 1
           , 3 /\ 1
           , 4 /\ 1
           , 5 /\ 1
           , 6 /\ 1
           ])
      (map (Tuple <$> _.index <*> _.groupNumber)
         $ Array.sortWith _.index
         $ connectedGroups
         $ Map.fromFoldable [ 0 /\ Set.fromFoldable [ 2 ]
                            , 1 /\ Set.fromFoldable [ 1 ]
                            , 2 /\ Set.fromFoldable [ 0, 3, 4 ]
                            , 3 /\ Set.fromFoldable [ 2, 4 ]
                            , 4 /\ Set.fromFoldable [ 2, 3, 6 ]
                            , 5 /\ Set.fromFoldable [ 6 ]
                            , 6 /\ Set.fromFoldable [ 4, 5 ]])

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
