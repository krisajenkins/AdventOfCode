module Year2017.Day2 where

import Prelude

import Control.Fold (foldl, maximum, minimum, sum)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.MonadPlus (guard)
import Data.Array as Array
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.String (char)

readInput :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (Array (Array Int))
readInput = mustSucceed =<< parseFile lineParser "src/Year2017/Day2.txt"

lineParser :: Parser (Array Int)
lineParser = Array.fromFoldable <$> integer `sepBy` char '\t'

------------------------------------------------------------

solution1 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) Int
solution1 = sumOfRanges <$> readInput

solution2 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) Int
solution2 = sumOfCleanDivisors <$> readInput

sumOfRanges :: Array (Array Int) -> Int
sumOfRanges values = foldl sum (rangeOf <$> values)
  where
    rangeOf = foldl ((-) <$> maximum <*> minimum)

sumOfCleanDivisors :: Array (Array Int) -> Int
sumOfCleanDivisors values = foldl sum (cleanDivisorsOf <$> values)

-- | Note: This code could be wrong if a line contains the same number
-- | twice. We're assuming that's not the case. ;-)
cleanDivisorsOf :: Array Int -> Int
cleanDivisorsOf xs = foldl maximum $ do
  a <- xs
  b <- xs
  guard (a /= b)
  pure $ if mod a b == 0
    then div a b
    else 0
