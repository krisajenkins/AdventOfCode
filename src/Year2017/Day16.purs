module Year2017.Day16 where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (catMaybes, index)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.String (trim)
import Data.String as String
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import ParserUtils (integer, mustSucceed)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.String (anyChar, char)
import Utils (inc, repeatN)

data Move
  = Spin Int
  | Exchange Int Int
  | Partner Char Char

derive instance genericMove :: Generic Move

instance showMove :: Show Move where
  show = gShow

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List Move)
readInput =
  (runParser movesParser <<< trim <$> readTextFile UTF8 "src/Year2017/Day16.txt")
    >>= mustSucceed

movesParser :: Parser (List Move)
movesParser = moveParser `sepBy` char ','

moveParser :: Parser Move
moveParser =
  (Spin <$> (char 's' *> integer))
  <|>
  (Exchange <$> (char 'x' *> integer) <*> (char '/' *> integer))
  <|>
  (Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar))

initialChars :: Array Char
initialChars = String.toCharArray "abcdefghijklmnop"

move :: Array Char -> Move -> Array Char
move state (Spin n) =
  Array.drop distance state <> Array.take distance state
  where
    distance = (Array.length state) - n
move state (Exchange x y) =
  Array.updateAtIndices (catMaybes [updateX, updateY]) state
  where
    updateX = Tuple <$> pure x <*> index state y
    updateY = Tuple <$> pure y <*> index state x
move state (Partner a b) =
  updater <$> state
  where
    updater n
      | n == a = b
      | n == b = a
      | otherwise = n

solution1 :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (Array Char)
solution1 = do
  List.foldl move initialChars <$> readInput

------------------------------------------------------------

summariseDance :: forall a t. Eq a => Traversable t => Array a -> t a -> Maybe (t Int)
summariseDance to from =
  traverse lookup from
  where
    lookup char = Array.elemIndex char to

type State =
  { steps :: Int
  , chars :: Array Char
  }

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Either String (Array Char))
solution2 = do
  moveList <- readInput
  oneDance <- solution1
  pure $ do
    -- Find the period with which the dance returns to the original state.
    let period = _.steps $ tailRec (findPeriod moveList) { steps: 1, chars: oneDance }
    -- We can now skip most a billion steps.
    let steps = mod (1000*1000*1000) period
    -- Finish up.
    pure $ repeatN steps (dance moveList) initialChars
  where
    findPeriod :: List Move -> State -> Step State State
    findPeriod moveList state@{ steps, chars } =
      if chars == initialChars
      then Done state
      else Loop { steps: inc steps
                , chars: List.foldl move chars moveList
                }

    dance :: List Move -> Array Char -> Array Char
    dance moveList chars =
      List.foldl move chars moveList
