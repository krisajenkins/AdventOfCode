module Year2017.Day18 where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Monad.State (class MonadState, execState)
import Data.Array as Array
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(Right, Left))
import Data.Lens (Lens', assign, assignJust, modifying, use, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.String (anyLetter, string)

data Instruction
  = Send Char
  | Set Char Target
  | Add Char Target
  | Multiply Char Target
  | Remainder Char Target
  | Receive Char
  | Jump Target Target

type Target = Either BigInt Char

derive instance eqInstruction :: Eq Instruction

newtype Memory = Memory
  { counter :: BigInt
  , registers :: Map Char BigInt
  , sending :: Maybe BigInt
  , received :: Maybe BigInt
  }


derive instance newtypeMemory :: Newtype Memory _
derive instance eqMemory :: Eq Memory

_counter :: Lens' Memory BigInt
_counter = _Newtype <<< prop (SProxy :: SProxy "counter")

_registers :: Lens' Memory (Map Char BigInt)
_registers = _Newtype <<< prop (SProxy :: SProxy "registers")

_sending :: Lens' Memory (Maybe BigInt)
_sending = _Newtype <<< prop (SProxy :: SProxy "sending")

_received :: Lens' Memory (Maybe BigInt)
_received = _Newtype <<< prop (SProxy :: SProxy "received")

initialMemory :: Memory
initialMemory = Memory
  { counter: zero
  , registers: Map.empty
  , sending: Nothing
  , received: Nothing
  }

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List Instruction)
readInput =
  parseFile (instructionParser <?> "Expected instruction") "src/Year2017/Day18.txt"
    >>= mustSucceed

instructionParser :: Parser Instruction
instructionParser =
  (Send <$> (string "snd " *> anyLetter) <?> "Expected a SND command")
  <|>
  (Set <$> (string "set " *> anyLetter) <*> (string " " *> targetParser) <?> "Expected a SET command")
  <|>
  (Add <$> (string "add " *> anyLetter) <*> (string " " *> targetParser) <?> "Expected a ADD command")
  <|>
  (Multiply <$> (string "mul " *> anyLetter) <*> (string " " *> targetParser) <?> "Expected a MUL command")
  <|>
  (Remainder <$> (string "mod " *> anyLetter) <*> (string " " *> targetParser) <?> "Expected a MOD command")
  <|>
  (Receive <$> (string "rcv " *> anyLetter) <?> "Expected a RCV command")
  <|>
  (Jump <$> (string "jgz " *> targetParser) <*> (string " " *> targetParser)<?> "Expected a JGZ command")

targetParser :: Parser Target
targetParser =
  (Left <<< fromInt <$> integer)
  <|>
  (Right <$> anyLetter)

------------------------------------------------------------

valueOf :: forall m. MonadState Memory m => Target -> m BigInt
valueOf (Left n) = pure n
valueOf (Right char) = fromMaybe zero <$> use (_registers <<< at char)

tickCounter :: forall m. MonadState Memory m => m Unit
tickCounter = modifying _counter ((+) one)

eval :: forall m. MonadState Memory m => Instruction -> m Unit
eval (Send char) = do
  val <- valueOf (Right char)
  assignJust _sending val
  tickCounter
eval (Receive char) = do
  val <- valueOf (Right char)
  sending <- use _sending
  when (val /= zero)
    (assign _received sending)
  tickCounter
eval (Jump source target) = do
  sourceValue <- valueOf source
  targetValue <- valueOf target
  modifying _counter ((+) (if sourceValue > zero
                           then targetValue
                           else one))
eval (Set char target) = do
  v <- valueOf target
  assignJust (_registers <<< at char) v
  tickCounter
eval (Add char target) = do
  v <- valueOf target
  modifying (_registers <<< at char) (Just <<< (+) v <<< fromMaybe zero)
  tickCounter
eval (Multiply char target) = do
  v <- valueOf target
  modifying (_registers <<< at char) (Just <<< (*) v <<< fromMaybe zero)
  tickCounter
eval (Remainder char target) = do
  v <- valueOf target
  modifying (_registers <<< at char) (Just <<< flip mod v <<< fromMaybe zero)
  tickCounter

runCPU :: Array Instruction -> Maybe BigInt
runCPU instructions =
  tailRec go initialMemory
  where
    go :: Memory -> Step Memory (Maybe BigInt)
    go memory =
      case view _received memory, Array.index instructions (degree (view _counter memory)) of
        Just r, _ -> Done $ Just r
        Nothing, Nothing -> Done Nothing
        Nothing, Just instruction -> Loop $ execState (eval instruction) memory

solution1 :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (Maybe BigInt)
solution1 = do
  runCPU <<< Array.fromFoldable <$> readInput

------------------------------------------------------------


solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    String
solution2 = do
  pure ""
