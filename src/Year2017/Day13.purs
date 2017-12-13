module Year2017.Day13 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)
import Utils (dec)

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Array (Int /\ Int))
readInput =
  parseFile lineParser "src/Year2017/Day13.txt" >>= mustSucceed

lineParser :: Parser (Int /\ Int)
lineParser = do
  scanner <- integer
  _ <- string ": "
  ranger <- integer
  pure $ scanner /\ ranger

scannerCost :: Int /\ Int -> Int
scannerCost (scanner /\ range) =
  if mod scanner (2 * (dec range)) == 0
  then scanner * range
  else 0

solution1 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) Int
solution1 =
  sum <<< map scannerCost <$> readInput

-- ~ Stealing a trick from the sieve of Erastosthenes
findSafeDelay :: Array (Int /\ Int) -> Array Int
findSafeDelay scanners =
  tailRec go (Array.range 0 5000000 /\ scanners)
  where
    go (sieve /\ scanners) =
      case Array.head scanners of
        Nothing -> Done sieve
        Just (scanner /\ range) ->
          let newSieve = (Array.filter (\n -> scannerCost (scanner + n /\ range) == 0) sieve)
          in Loop $ (newSieve
                     /\
                     (Array.drop 1 scanners))

solution2 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) (Maybe Int)
solution2 = do
  Array.head <<< findSafeDelay <$> readInput
