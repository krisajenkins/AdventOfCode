module Year2017.Day16Test where

import Prelude

import Year2017.Day16 (Move(..), move, solution1, solution2)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.String as String
import Node.FS (FS)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)

all :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
all =
  suite "Day16" do
    moveTests
    solution1Tests
    solution2Tests

moveTests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
moveTests =
  test "move" do
    equal "deabc" (String.fromCharArray $ move (String.toCharArray "abcde") (Spin 2))
    equal "adcbe" (String.fromCharArray $ move (String.toCharArray "abcde") (Exchange 1 3))
    equal "dbcae" (String.fromCharArray $ move (String.toCharArray "abcde") (Partner 'a' 'd'))

solution1Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution1Tests =
  test "Solution 1" do
    answer <- String.fromCharArray <$> liftEff solution1
    equal "olgejankfhbmpidc"  answer

solution2Tests :: forall eff. TestSuite (testOutput :: TESTOUTPUT, now :: NOW, fs :: FS, exception :: EXCEPTION | eff)
solution2Tests =
  test "Solution 2" do
    answer <- rmap String.fromCharArray <$> liftEff solution2
    equal (Right "gfabehpdojkcimnl") answer
