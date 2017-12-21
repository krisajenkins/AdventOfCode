module Year2017.Day20Test where

import Prelude

import Data.List as List
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Year2017.Day20 (Coord(..), IntersectionTime(..), Particle(..), findIntersections, findSurvivors, readInput, solution1, solution2)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day20" do
    readInputTests
    findIntersectionsTests
    findSurvivorsTests
    solution1Tests
    solution2Tests

readInputTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
readInputTests =
  test "readInput" do
    input <- liftEff readInput
    equal 1000 (List.length input)

p0 :: Particle
p0 = Particle { position: Coord { x: (-6), y: 0, z: 0 }
              , velocity: Coord { x: 3, y: 0, z: 0 }
              , acceleration: Coord { x: 0, y: 0, z: 0 }
              }

p1 :: Particle
p1 = Particle { position: Coord { x: (-4), y: 0, z: 0 }
              , velocity: Coord { x: 2, y: 0, z: 0 }
              , acceleration: Coord { x: 0, y: 0, z: 0 }
              }

p2 :: Particle
p2 = Particle { position: Coord { x: (-2), y: 0, z: 0 }
              , velocity: Coord { x: 1, y: 0, z: 0 }
              , acceleration: Coord { x: 0, y: 0, z: 0 }
              }

p3 :: Particle
p3 = Particle { position: Coord { x: 3, y: 0, z: 0 }
              , velocity: Coord { x: (-1), y: 0, z: 0 }
              , acceleration: Coord { x: 0, y: 0, z: 0 }
              }

p4 :: Particle
p4 = Particle { position: Coord { x: (-10), y: 0, z: 0 }
              , velocity: Coord { x: 0, y: 0, z: 0 }
              , acceleration: Coord { x: 1, y: 0, z: 0 }
              }

findIntersectionsTests :: forall eff. TestSuite eff
findIntersectionsTests =
  suite "findIntersections" do
    test "simple clash 1" do
      equal [Once 2.0, Always, Always]
        (findIntersections p0 p1)
    test "simple clash 2" do
      equal [Once 2.0, Always, Always]
        (findIntersections p1 p2)

findSurvivorsTests :: forall eff. TestSuite eff
findSurvivorsTests =
  suite "findSurvivors" do
    test "Simplified AoC Example" do
      let example = findSurvivors (List.fromFoldable [ p0, p1 ])
      equal 0 (List.length example)
    test "Simplified AoC Example 2" do
      let example = findSurvivors (List.fromFoldable [ p0, p3 ])
      equal 0 (List.length example)
    test "AoC Example" do
      let example = findSurvivors (List.fromFoldable [ p0, p1, p2, p3 ])
      equal 0 (List.length example)

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal (Just 300) answer

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 502 answer
