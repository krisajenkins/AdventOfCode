module Year2017.Day22 where

import Prelude

import Compass (Avatar(..), Cmd(..), Direction(..), _x, _y, handleCmd)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Data.Lens (view)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Node.FS (FS)
import ParserUtils (mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (char)
import Utils (dec, inc, showWorld)

type World a = Map (Int /\ Int) a

toWorld :: forall a. List (List a) -> World a
toWorld =
  Map.fromFoldable <<<
  List.concat <<< List.mapWithIndex (\row -> List.mapWithIndex (\col -> Tuple (col /\ row) ))

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (World Boolean)
readInput =
  toWorld <$> (parseFile (many1 cellParser) "src/Year2017/Day22.txt" >>= mustSucceed)

cellParser :: Parser Boolean
cellParser =
  (char '#' *> pure true)
  <|>
  (char '.' *> pure false)

type State a =
  { count :: Int
  , changeCount :: Map (Maybe a) Int
  , world :: World a
  , avatar :: Avatar
  }

evolve :: forall a.
  Ord a =>
  (Maybe a -> Maybe a)
  -> (Maybe a -> Avatar -> Avatar)
  -> World a
  -> Int
  -> State a
evolve changeState changeAvatar world count =
  tailRec go { count, changeCount: Map.empty, world, avatar: Avatar { x: 12, y: 12, direction: North } }
  where
    go state@{count: 0} = Done state
    go {count: n, changeCount, world, avatar } =
      let position = (view _x avatar /\ view _y avatar)
          health = Map.lookup position world
          avatar' = changeAvatar health avatar
          world' = Map.alter changeState position world
      in Loop { count: dec n
              , world: world'
              , avatar: avatar'
              , changeCount: Map.alter (Just <<< inc <<< fromMaybe 0) (changeState health) changeCount
              }

solution1 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solution1 = do
  input <- readInput
  let final = evolve changeState changeAvatar input 10000
  showWorld formatter 30 final.world
  logShow final.avatar
  pure $ fromMaybe 0 $ Map.lookup (Just true) final.changeCount
  where
    formatter true = '#'
    formatter false = '.'
    changeState Nothing = changeState (Just false)
    changeState (Just true) = Nothing
    changeState (Just false) = Just true
    changeAvatar :: Maybe Boolean -> Avatar -> Avatar
    changeAvatar (Just true) = handleCmd TurnRight >>> handleCmd (Move 1)
    changeAvatar (Just false) = handleCmd TurnLeft >>> handleCmd (Move 1)
    changeAvatar Nothing = changeAvatar (Just false)

data Health = Clean | Weakened | Infected | Flagged
derive instance eqHealth :: Eq Health
derive instance ordHealth :: Ord Health

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff)
    Int
solution2 = do
  input <- map (\x -> if x then Infected else Clean) <$> readInput
  let final = evolve changeState changeAvatar input 10000000
  showWorld formatter 30 final.world
  logShow final.avatar
  pure $ fromMaybe 0 $ Map.lookup (Just Infected) final.changeCount
  where
    formatter Clean = '.'
    formatter Weakened = 'W'
    formatter Infected = '#'
    formatter Flagged = 'o'
    changeState Nothing = changeState (Just Clean)
    changeState (Just Clean) = Just Weakened
    changeState (Just Weakened) = Just Infected
    changeState (Just Infected) = Just Flagged
    changeState (Just Flagged) = Nothing
    changeAvatar :: Maybe Health -> Avatar -> Avatar
    changeAvatar (Just Clean) = handleCmd TurnLeft >>> handleCmd (Move 1)
    changeAvatar (Just Weakened) = handleCmd (Move 1)
    changeAvatar (Just Infected) = handleCmd TurnRight >>> handleCmd (Move 1)
    changeAvatar (Just Flagged) = handleCmd TurnLeft >>> handleCmd TurnLeft >>> handleCmd (Move 1)
    changeAvatar Nothing = changeAvatar (Just Clean)
