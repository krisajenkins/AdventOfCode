module Year2017.Day13 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (sum)
import Data.List (List(..), catMaybes)
import Data.List as List
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)
import Utils (dec)

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List (Int /\ Int))
readInput =
  parseFile lineParser "src/Year2017/Day13.txt" >>= mustSucceed

lineParser :: Parser (Int /\ Int)
lineParser = do
  scanner <- integer
  _ <- string ": "
  range <- integer
  pure $ scanner /\ range

scannerCost :: Int /\ Int -> Maybe Int
scannerCost (scanner /\ range) =
  if mod scanner (2 * (dec range)) == 0
  then Just (scanner * range)
  else Nothing

solution1 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) Int
solution1 =
  sum <<< catMaybes <<< map scannerCost <$> readInput

-- | Stealing a trick from the sieve of Erastosthenes
findSafeDelay :: List (Int /\ Int) -> Array Int
findSafeDelay scanners =
  tailRec go (Array.range 0 4000000 /\ scanners)
  where
    go (sieve /\ Nil) = Done sieve
    go (sieve /\ (Cons (scanner /\ range) scanners)) =
      let newSieve = (Array.filter (\n -> scannerCost (scanner + n /\ range) == Nothing) sieve)
      in Loop $ newSieve /\ scanners

solution2 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) (Maybe Int)
solution2 =
  Array.head <<< findSafeDelay <$> readInput
