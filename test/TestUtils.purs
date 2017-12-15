module TestUtils where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Test.Unit.Console (TESTOUTPUT, printLabel)

timeEff :: forall eff a b.
  (a -> Eff (now :: NOW, testOutput :: TESTOUTPUT | eff) b)
  -> a
  -> Eff (now :: NOW, testOutput :: TESTOUTPUT | eff) b
timeEff action value = do
  before <- now
  result <- action value
  after <- now
  printLabel $ "Took: " <> show ((unwrap (unInstant after)) - (unwrap (unInstant before))) <> "ms"
  pure result
