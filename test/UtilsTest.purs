module UtilsTest where

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
import Utils
import Year2017.Day12 as Day12

all :: forall eff. TestSuite eff
all =
  suite "Utils" do
    connectedGroupsTests

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
         $ connectedGroups Day12.walker
         $ Map.fromFoldable [ 0 /\ Set.fromFoldable [ 2 ]
                            , 1 /\ Set.fromFoldable [ 1 ]
                            , 2 /\ Set.fromFoldable [ 0, 3, 4 ]
                            , 3 /\ Set.fromFoldable [ 2, 4 ]
                            , 4 /\ Set.fromFoldable [ 2, 3, 6 ]
                            , 5 /\ Set.fromFoldable [ 6 ]
                            , 6 /\ Set.fromFoldable [ 4, 5 ]])
