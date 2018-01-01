module Year2017.Day22 where

import Prelude

import Compass (Avatar(Avatar), Cmd(Move, TurnLeft, TurnRight), Direction(North), _x, _y, handleCmds)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.DefaultMap (DefaultMap)
import Data.DefaultMap as DefaultMap
import Data.Lens (view)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS (FS)
import ParserUtils (mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (char)
import Utils (inc, repeatN)

type World a = DefaultMap (Int /\ Int) a

toWorld :: forall a. a -> List (List a) -> World a
toWorld defaultValue =
  List.mapWithIndex (\row -> List.mapWithIndex (\col -> Tuple (col /\ row)))
  >>> List.concat
  >>> DefaultMap.fromFoldable defaultValue

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List (List Boolean))
readInput =
  parseFile (many1 cellParser) "src/Year2017/Day22.txt"
    >>= mustSucceed

cellParser :: Parser Boolean
cellParser =
  (char '#' *> pure true)
  <|>
  (char '.' *> pure false)

type State a =
  { avatar :: Avatar
  , world :: World a
  , changeTally :: DefaultMap a Int
  }

class Langton a where
  formatter :: a -> Char
  changeState :: a -> a
  avatarCmds :: a -> Array Cmd

evolve :: forall a.
  Ord a
  => Langton a
  => Int
  -> World a
  -> State a
evolve count world =
  repeatN count step (initialState world)

initialState :: forall a. World a -> State a
initialState world =
  { changeTally: DefaultMap.empty 0
  , world
  , avatar: Avatar { x: 12
                   , y: 12
                   , direction: North
                   }
  }

step :: forall a.
  Langton a
  => Ord a
  => State a
  -> State a
step { changeTally, world, avatar } =
  let position = (Tuple <$> view _x <*> view _y) avatar
      health = DefaultMap.lookup position world
      newAvatar = handleCmds avatar (avatarCmds health)
      newWorld = DefaultMap.alter (Just <<< changeState) position world
      newChangeTally = DefaultMap.alter
                         (Just <<< inc)
                         (changeState health)
                         changeTally
  in { world: newWorld
     , avatar: newAvatar
     , changeTally: newChangeTally
     }

instance langtonBoolean :: Langton Boolean where
  formatter true = '#'
  formatter false = '.'

  changeState x = not x

  avatarCmds true = [TurnRight, Move 1]
  avatarCmds false = [TurnLeft, Move 1]

solution1 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solution1 = do
  input <- toWorld false <$> readInput
  let final = evolve 10000 input
  pure $ DefaultMap.lookup true final.changeTally

data Health = Clean | Weakened | Infected | Flagged
derive instance eqHealth :: Eq Health
derive instance ordHealth :: Ord Health

instance langtonHealth :: Langton Health where
  formatter Clean = '.'
  formatter Weakened = 'W'
  formatter Infected = '#'
  formatter Flagged = 'o'

  changeState Clean = Weakened
  changeState Weakened = Infected
  changeState Infected = Flagged
  changeState Flagged = Clean

  avatarCmds Clean = [TurnLeft, Move 1]
  avatarCmds Weakened = [Move 1]
  avatarCmds Infected = [TurnRight, Move 1]
  avatarCmds Flagged = [TurnLeft, TurnLeft, Move 1]

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solution2 = do
  input <- toWorld Clean <<< map (map (\x -> if x then Infected else Clean)) <$> readInput
  let final = evolve 10000000 input
  pure $ DefaultMap.lookup Infected final.changeTally
