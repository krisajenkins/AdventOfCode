module Year2017.Day11 where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (class Foldable, foldl, sum)
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.Ord (abs)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import ParserUtils (mustSucceed)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.String (char, string)

data Direction = NW | N | NE | SW | S | SE

derive instance genericDirection :: Generic Direction

data Point = Point Int Int Int

derive instance genericPoint :: Generic Point

instance showPoint :: Show Point where
  show = gShow

instance semiringPoint :: Semiring Point where
  zero = Point 0 0 0
  one = Point 1 1 1
  add (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)
  mul (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 * x2) (y1 * y2) (z1 * z2)

instance ringPoint :: Ring Point where
  sub (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 - x2) (y1 - y2) (z1 - z2)

directionParser :: Parser Direction
directionParser =
  (string "nw" *> pure NW)
  <|>
  (string "ne" *> pure NE)
  <|>
  (string "n" *> pure N)
  <|>
  (string "sw" *> pure SW)
  <|>
  (string "se" *> pure SE)
  <|>
  (string "s" *> pure S)

inputParser :: Parser (List Direction)
inputParser = directionParser `sepBy` char ','

readInput :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (List Direction)
readInput =
  runParser inputParser <$> readTextFile UTF8 "src/Year2017/Day11.txt"
    >>= mustSucceed

toPoint :: Direction -> Point
toPoint NW = Point (-1)  1   0
toPoint N  = Point   0   1 (-1)
toPoint NE = Point   1   0 (-1)
toPoint SW = negate $ toPoint NE
toPoint S  = negate $ toPoint N
toPoint SE = negate $ toPoint NW

walk :: forall f. Foldable f => Functor f => f Direction -> Point
walk = sum <<< map toPoint

distance :: Point -> Int
distance (Point x y z) =
  (abs x + abs y + abs z) / 2

solution1 :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) Int
solution1 =
  distance <<< walk <$> readInput

solution2 :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) Int
solution2 = do
  input <- readInput
  pure $ distance $ snd $ foldl furthest zero $ map toPoint input
  where
    furthest (current /\ best) step =
      let new = current + step
      in if distance new > distance best
         then new /\ new
         else new /\ best
