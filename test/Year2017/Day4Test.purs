module Year2017.Day4Test where

import Prelude

import Control.Fold (foldl)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Year2017.Day4 (containsDuplicates)

all :: forall eff. TestSuite eff
all =
  suite "Day4" do
    containsDuplicatesTests

containsDuplicatesTests :: forall eff. TestSuite eff
containsDuplicatesTests =
  test "Contains Duplicates Folds" do
    assert "Array with duplicates" (foldl containsDuplicates [1, 4, 5, 4])
    assertFalse "Array without duplicates" (foldl containsDuplicates [2, 4, 5])
