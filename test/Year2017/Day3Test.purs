module Year2017.Day3Test where

import Prelude

import Data.Traversable (sequence_)
import Data.Tuple.Nested ((/\))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Year2017.Day3 (firstLarger, positionOf)

all :: forall eff. TestSuite eff
all =
  suite "Day3" do
    positionOfTests
    firstLargerTests

positionOfTests :: forall eff. TestSuite eff
positionOfTests = do
  test "Solution 1 Distance" do
    sequence_ $ matches <$>
      [ 1 /\ (0 /\ 0)
      , 2 /\ (1 /\ 0)
      , 3 /\ (1 /\ -1)
      , 4 /\ (0 /\ -1)
      , 5 /\ (-1 /\ -1)
      , 6 /\ (-1 /\ 0)
      , 7 /\ (-1 /\ 1)
      , 8 /\ (0 /\ 1)
      , 9 /\ (1 /\ 1)
      , 10 /\ (2 /\ 1)
      , 11 /\ (2 /\ 0)
      , 12 /\ (2 /\ -1)
      , 13 /\ (2 /\ -2)
      , 14 /\ (1 /\ -2)
      , 15 /\ (0 /\ -2)
      , 16 /\ (-1 /\ -2)
      , 17 /\ (-2 /\ -2)
      , 18 /\ (-2 /\ -1)
      , 19 /\ (-2 /\ 0)
      , 23 /\ (0 /\ 2)
      , 25 /\ (2 /\ 2)
      , 26 /\ (3 /\ 2)
      , 1024 /\ (-15 /\ -16)
      ]
    where
      matches (n /\ position) =
        equal (n /\ position) (n /\ positionOf n)

firstLargerTests :: forall eff. TestSuite eff
firstLargerTests = do
  test "Solution 2 Distance" do
    equal 23 (firstLarger 20)
    equal 806 (firstLarger 800)
