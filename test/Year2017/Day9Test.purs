module Year2017.Day9Test where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Text.Parsing.StringParser (runParser)
import Year2017.Day9 (Tree(..), groupParser, solution1, solution2)

all :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day9" do
    groupParserTests
    solution1Tests
    solution2Tests

groupParserTests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
groupParserTests =
  test "groupParser" do
    equal
      (Right (Node [Node [Leaf "a},{<a},{<a},{<ab"]]))
      (runParser groupParser "{{<a!>},{<a!>},{<a!>},{<ab>}}")
    equal
      (Right (Node [Node [Leaf "ab"], Node [Leaf "ab"], Node [Leaf "ab"], Node [Leaf "ab"]]))
      (runParser groupParser "{{<ab>},{<ab>},{<ab>},{<ab>}}")


solution1Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- liftEff solution1
    equal 11347 answer

solution2Tests :: forall eff. TestSuite (fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- liftEff solution2
    equal 5404 answer
