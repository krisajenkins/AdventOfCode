module Year2017.Day8 where

import Prelude

import Control.Alternative ((<|>))
import Control.Fold (Fold, foldl, maximum, scanl, unfoldFold_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String (fromCharArray)
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1, (<?>))
import Text.Parsing.StringParser.String (alphaNum, skipSpaces, string)

newtype Instruction = Instruction
  { register :: String
  , direction :: Direction
  , value :: Int
  , condition :: Condition
  }

derive instance genericInstruction :: Generic Instruction
derive instance newtypeInstruction :: Newtype Instruction _
derive instance eqInstruction :: Eq Instruction

instance showInstruction :: Show Instruction where
  show = gShow

data Direction = Inc | Dec

derive instance eqDirection :: Eq Direction
derive instance genericDirection :: Generic Direction

-- | Some names chosen to not clash with Data.Ord.
data Operation = Lt | Lte | Gt | Gte | Ne | Same

derive instance eqOperation :: Eq Operation
derive instance genericOperation :: Generic Operation

newtype Condition = Condition
  { register :: String
  , operation :: Operation
  , value :: Int
  }

derive instance eqCondition :: Eq Condition
derive instance genericCondition :: Generic Condition
instance showCondition :: Show Condition where
  show = gShow

------------------------------------------------------------

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Array Instruction)
readInput =
  parseFile lineParser "src/Year2017/Day8.txt"
    >>= mustSucceed

lineParser :: Parser Instruction
lineParser = do
  register <- nameParser
  skipSpaces
  direction <- directionParser
  skipSpaces
  value <- integer
  skipSpaces
  condition <- conditionParser <?> "Expected condition"
  pure $ Instruction {register, direction, value, condition}

directionParser :: Parser Direction
directionParser =
  ( string "inc" *> pure Inc)
  <|>
  ( string "dec" *> pure Dec)

nameParser :: Parser String
nameParser = fromCharArray <<< Array.fromFoldable <$> many1 alphaNum

conditionParser :: Parser Condition
conditionParser = do
  _ <- string "if"
  skipSpaces
  register <- nameParser
  skipSpaces
  operation <- operationParser
  skipSpaces
  value <- integer
  pure $ Condition { register, operation, value }

operationParser :: Parser Operation
operationParser =
  ((string "<=" *> pure Lte)
   <|>
   (string "<" *> pure Lt)
   <|>
   (string ">=" *> pure Gte)
   <|>
   (string ">" *> pure Gt)
   <|>
   (string "!=" *> pure Ne)
   <|>
   (string "==" *> pure Same)) <?> "Expected operation"

------------------------------------------------------------

type Registers = Map String Int

evalCondition :: Registers -> Condition -> Boolean
evalCondition registers (Condition { register, operation, value }) =
  case operation of
    Gt -> registerValue > value
    Gte -> registerValue >= value
    Lt -> registerValue < value
    Lte -> registerValue <= value
    Ne -> registerValue /= value
    Same -> registerValue == value
  where registerValue = fromMaybe 0 $ Map.lookup register registers

evalInstruction :: Registers -> Instruction -> Registers
evalInstruction registers (Instruction {register, direction, value, condition}) =
  if evalCondition registers condition
    then Map.alter (Just <<< directionFn direction <<< fromMaybe 0) register registers
    else registers
  where
    directionFn Dec x = x - value
    directionFn Inc x = x + value

foldCPU :: Fold Instruction Registers
foldCPU = unfoldFold_ Map.empty evalInstruction

solution1 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) Int
solution1 = finalMaximum <$> readInput
  where
    finalMaximum =
      foldl foldCPU
        >>> Map.values
        >>> foldl maximum

-- | Whilst this isn't the most efficient way to solve the problem,
-- | Control.Fold does give us a pleasing symmetry between 'Final value
-- | of the registers' and 'lifetime value of the registers', through
-- | the difference between foldl and scanl.
solution2 :: forall eff. Eff (fs :: FS , exception :: EXCEPTION | eff) Int
solution2 = lifetimeMaximum <$> readInput
  where
    lifetimeMaximum =
      scanl foldCPU
        >>> map Map.values
        >>> Array.concatMap Array.fromFoldable
        >>> foldl maximum
