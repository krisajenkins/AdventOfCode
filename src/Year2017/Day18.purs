module Year2017.Day18 where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Monad.State (class MonadState, evalState, execState)
import Data.Array (uncons, snoc)
import Data.Array as Array
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(Right, Left))
import Data.Lens (Lens', set, assign, assignJust, modifying, over, to, use, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.String (anyLetter, string)

data Instruction
  = Send Target
  | Set Char Target
  | Add Char Target
  | Multiply Char Target
  | Remainder Char Target
  | Receive Char
  | Jump Target Target
  | Skip

type Target = Either BigInt Char

derive instance eqInstruction :: Eq Instruction
instance showInstruction :: Show Instruction where
  show (Send target) = "Send " <> show target
  show (Set char target) = "Set " <> show char <> " " <> show target
  show (Add char target) = "Add " <> show char <> " " <> show target
  show (Multiply char target) = "Multiply " <> show char <> " " <> show target
  show (Remainder char target) = "Remainder " <> show char <> " " <> show target
  show (Receive target) = "Receive " <> show target
  show (Jump char target) = "Jump " <> show char <> " " <> show target
  show Skip = "Skip"

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

initialMemory :: BigInt -> Memory
initialMemory cpuId = Memory
  { counter: zero
  , registers: Map.singleton 'p' cpuId
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
  (Send <$> (string "snd " *> targetParser) <?> "Expected a SND command")
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
eval (Send target) = do
  val <- valueOf target
  assignJust _sending val
  tickCounter
eval (Receive char) = do
  val <- valueOf (Right char)
  sending <- use _sending
  when (val /= zero)
    (assign _received sending)
  tickCounter
eval Skip = do
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
  tailRec go (initialMemory zero)
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

newtype Core = Core
  { memory :: Memory
  , buffer :: Array BigInt
  , sendCount :: BigInt
  }

derive instance newtypeCore :: Newtype Core _
derive instance eqCore :: Eq Core

newtype DualCore = DualCore
  { cpu0 :: Core
  , cpu1 :: Core
  }

derive instance newtypeDualCore :: Newtype DualCore _
derive instance eqDualCore :: Eq DualCore

_cpu0 :: Lens' DualCore Core
_cpu0 = _Newtype <<< prop (SProxy :: SProxy "cpu0")

_cpu1 :: Lens' DualCore Core
_cpu1 = _Newtype <<< prop (SProxy :: SProxy "cpu1")

_memory :: Lens' Core Memory
_memory = _Newtype <<< prop (SProxy :: SProxy "memory")

_buffer :: Lens' Core (Array BigInt)
_buffer = _Newtype <<< prop (SProxy :: SProxy "buffer")

_sendCount :: Lens' Core BigInt
_sendCount = _Newtype <<< prop (SProxy :: SProxy "sendCount")

runDualCore :: Array Instruction -> (BigInt /\ BigInt)
runDualCore instructions =
  tailRec go $ DualCore { cpu0: Core { memory: initialMemory zero
                                     , buffer: []
                                     , sendCount: zero
                                     }
                        , cpu1: Core { memory: initialMemory one
                                     , buffer: []
                                     , sendCount: zero
                                     }
                        }
  where
    go state =
      let
          nextInstruction =
            Array.index instructions <<< view (_memory <<< _counter <<< to degree)
          halt = Done (view (_cpu0 <<< _sendCount) state /\ view (_cpu1 <<< _sendCount) state)
          handleSend :: Target -> Lens' DualCore Core -> DualCore -> DualCore
          handleSend target lens state =
            let value = evalState (valueOf target) (view (lens <<< _memory) state)
            in
            state
            # over (lens <<< _buffer) (flip snoc value)
            # over (lens <<< _sendCount) (add one)
            # over (lens <<< _memory) (execState (eval Skip))
          handleReceive :: Char -> Lens' DualCore Core -> Lens' DualCore Core -> DualCore -> DualCore
          handleReceive target from to state =
            case uncons (view (from <<< _buffer) state) of
              Nothing -> state
              Just { head: x, tail: xs } ->
                state
                # set (from <<< _buffer) xs
                # over (to <<< _memory) (execState (eval (Set target (Left x))))
      in
        case nextInstruction (view _cpu0 state), nextInstruction (view _cpu1 state), (view (_cpu0 <<< _buffer) state), (view (_cpu1 <<< _buffer) state) of
          Nothing, Nothing, _, _ -> halt

          Just (Send source), _, _, _ ->
            state
            # handleSend source _cpu0
            # Loop

          _, Just (Send source), _, _ ->
            state
            # handleSend source _cpu1
            # Loop

          Just (Receive _), Just (Receive _), [], [] -> halt

          Just (Receive destination1), Just (Receive destination2), _, _->
            state
            # handleReceive destination1 _cpu1 _cpu0
            # handleReceive destination2 _cpu0 _cpu1
            # Loop
          Just (Receive destination), Just i1, _, _->
            state
            # handleReceive destination _cpu1 _cpu0
            # over (_cpu1 <<< _memory) (execState (eval i1))
            # Loop
          Just i0, Just (Receive destination), _, _ ->
            state
            # handleReceive destination _cpu0 _cpu1
            # over (_cpu0 <<< _memory) (execState (eval i0))
            # Loop
          Just i0, Just i1, _, _ ->
            state
            # over (_cpu0 <<< _memory) (execState (eval i0))
            # over (_cpu1 <<< _memory) (execState (eval i1))
            # Loop
          Just i0, Nothing, _, _ ->
            state
            # over (_cpu0 <<< _memory) (execState (eval i0))
            # Loop
          Nothing, Just i1, _, _ ->
            state
            # over (_cpu1 <<< _memory) (execState (eval i1))
            # Loop

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (BigInt /\ BigInt)
solution2 = do
  runDualCore <<< Array.fromFoldable <$> readInput
