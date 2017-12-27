module Year2017.Day25 where

import Prelude hiding (between)

import Data.Lens (Lens', (^.), over, preview, set, view)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Data.BigInt (BigInt)
import Data.Generic (class Generic, gShow)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import ParserUtils (integer, mustSucceed)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (between, choice, many, withError)
import Text.Parsing.StringParser.String (anyLetter, char, string, whiteSpace)

newtype Program = Program
  { startingState :: Char
  , maxSteps :: Int
  , states :: Map Char Choice
  }

derive instance newtypeProgram :: Newtype Program _
derive instance eqProgram :: Eq Program

_startingState :: Lens' Program Char
_startingState = _Newtype <<< prop (SProxy :: SProxy "startingState")

_maxSteps :: Lens' Program Int
_maxSteps = _Newtype <<< prop (SProxy :: SProxy "maxSteps")

_states :: Lens' Program (Map Char Choice)
_states = _Newtype <<< prop (SProxy :: SProxy "states")

data Bit = One | Zero
derive instance genericBit :: Generic Bit
derive instance eqBit :: Eq Bit

data Instruction
  = WriteValue Bit
  | MoveLeft
  | MoveRight
  | ChangeToState Char

derive instance genericInstruction :: Generic Instruction
derive instance eqInstruction :: Eq Instruction
instance showInstruction :: Show Instruction where
  show = gShow

newtype Choice = Choice
  { ifOne :: List Instruction
  , ifZero :: List Instruction
  }

_ifOne :: Lens' Choice (List Instruction)
_ifOne = _Newtype <<< prop (SProxy :: SProxy "ifOne")

_ifZero :: Lens' Choice (List Instruction)
_ifZero = _Newtype <<< prop (SProxy :: SProxy "ifZero")

derive instance genericChoice :: Generic Choice
derive instance newtypeChoice :: Newtype Choice _
derive instance eqChoice :: Eq Choice
instance showChoice :: Show Choice where
  show = gShow


------------------------------------------------------------

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    Program
readInput =
  runParser programParser <<< trim <$> readTextFile UTF8 "src/Year2017/Day25.txt"
    >>= mustSucceed

programParser :: Parser Program
programParser = do
  startingState <- string "Begin in state " *> anyLetter <* string "."
  void whiteSpace
  maxSteps <- string "Perform a diagnostic checksum after " *> integer <* string " steps."
  void whiteSpace
  states <- Map.fromFoldable <$> (many statesParser)
  pure $ Program {startingState, maxSteps, states}

statesParser :: Parser (Tuple Char Choice)
statesParser = do
  state <- string "In state " *> anyLetter <* string ":"
  void whiteSpace
  ifZero <- do void $ string "If the current value is 0:"
               void whiteSpace
               many instructionParser
  void whiteSpace
  ifOne <- do void $ string "If the current value is 1:"
              void whiteSpace
              many instructionParser
  void whiteSpace
  pure $ Tuple state $ Choice {ifZero,ifOne}

instructionParser :: Parser Instruction
instructionParser =
  between whiteSpace whiteSpace
  ((choice [ pure WriteValue <*> (string "- Write the value " *> bitParser <* string ".")
           , pure MoveRight <* string "- Move one slot to the right."
           , pure MoveLeft <* string "- Move one slot to the left."
           , pure ChangeToState <*> (string "- Continue with state " *> anyLetter <* string ".")])
   `withError` "Expected instruction")

bitParser :: Parser Bit
bitParser =
  (char '0' *> pure Zero)
  <|>
  (char '1' *> pure One)

------------------------------------------------------------

newtype Machine = Machine
  { currentAddress :: BigInt
  , memory :: Set BigInt
  , currentState :: Char
  , stepCount :: Int
  }

derive instance newtypeMachine :: Newtype Machine _
derive instance eqMachine :: Eq Machine

_currentAddress :: Lens' Machine BigInt
_currentAddress = _Newtype <<< prop (SProxy :: SProxy "currentAddress")

_memory :: Lens' Machine (Set BigInt)
_memory = _Newtype <<< prop (SProxy :: SProxy "memory")

_currentState :: Lens' Machine Char
_currentState = _Newtype <<< prop (SProxy :: SProxy "currentState")

_stepCount :: Lens' Machine Int
_stepCount = _Newtype <<< prop (SProxy :: SProxy "stepCount")

handleInstruction :: Machine -> Instruction -> Machine
handleInstruction machine (WriteValue One) =
  over _memory (Set.insert (view _currentAddress machine)) machine
handleInstruction machine (WriteValue Zero) =
  over _memory (Set.delete (view _currentAddress machine)) machine
handleInstruction machine MoveLeft = over _currentAddress (sub one) machine
handleInstruction machine MoveRight = over _currentAddress (add one) machine
handleInstruction machine (ChangeToState newState) = set _currentState newState machine

runMachine :: Program -> Machine
runMachine program =
  tailRec go (Machine { currentAddress: zero
                      , memory: Set.empty
                      , currentState: view _startingState program
                      , stepCount: zero
                      } )
  where
    go :: Machine -> Step Machine Machine
    go machine =
      if (view _stepCount machine) == (view _maxSteps program)
      then Done machine
      else case preview (_states <<< ix (view _currentState machine)) program of
             Nothing -> Done machine
             Just choice ->
               let instructions =
                     choice ^. (if Set.member (view _currentAddress machine) (view _memory machine)
                                  then _ifOne
                                  else _ifZero)
               in List.foldl handleInstruction machine instructions
                  # over _stepCount (add one)
                  # Loop

solution1 :: forall eff.
  Eff
    (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | eff)
    Int
solution1 = do
  program <- readInput
  let machine = runMachine program
  pure $ Set.size $ view _memory machine

solution2 :: forall eff.
  Eff
    (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | eff)
    Int
solution2 = do
  pure 0
