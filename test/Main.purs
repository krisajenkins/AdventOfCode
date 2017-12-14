module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.ST (ST)
import Node.FS (FS)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Year2017.Day10Test as Year2017.Day10Test
import Year2017.Day11Test as Year2017.Day11Test
import Year2017.Day12Test as Year2017.Day12Test
import Year2017.Day13Test as Year2017.Day13Test
import Year2017.Day14Test as Year2017.Day14Test
import Year2017.Day1Test as Year2017.Day1Test
import Year2017.Day2Test as Year2017.Day2Test
import Year2017.Day3Test as Year2017.Day3Test
import Year2017.Day4Test as Year2017.Day4Test
import Year2017.Day5Test as Year2017.Day5Test
import Year2017.Day6Test as Year2017.Day6Test
import Year2017.Day7Test as Year2017.Day7Test
import Year2017.Day8Test as Year2017.Day8Test
import Year2017.Day9Test as Year2017.Day9Test
import UtilsTest as UtilsTest

main ::
  forall h eff.
  Eff ( testOutput :: TESTOUTPUT
      , avar :: AVAR
      , now :: NOW
      , console :: CONSOLE
      , fs :: FS
      , st :: ST h
      , exception :: EXCEPTION | eff
      ) Unit
main = runTest do
  UtilsTest.all
  Year2017.Day1Test.all
  Year2017.Day2Test.all
  Year2017.Day3Test.all
  Year2017.Day4Test.all
  Year2017.Day5Test.all
  Year2017.Day6Test.all
  Year2017.Day7Test.all
  Year2017.Day8Test.all
  Year2017.Day9Test.all
  Year2017.Day10Test.all
  Year2017.Day11Test.all
  Year2017.Day12Test.all
  Year2017.Day13Test.all
  Year2017.Day14Test.all
