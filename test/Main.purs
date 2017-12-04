module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Year2017.Day3Test as Year2017.Day3Test
import Year2017.Day4Test as Year2017.Day4Test

main :: Eff (testOutput :: TESTOUTPUT, avar :: AVAR, console :: CONSOLE) Unit
main = runTest do
  Year2017.Day3Test.all
  Year2017.Day4Test.all
