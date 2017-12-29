module Year2017.Day19 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.String as String
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)

type World = Map (Int /\ Int) Char

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    World
readInput = do
  toWorld <$> readTextFile UTF8 "src/Year2017/Day19.txt"

toWorld :: String -> World
toWorld text =
  Map.fromFoldable
  $ Array.filter (\item -> (snd item) /= ' ')
  $ Array.concat
  $ Array.zipWith slurpLine rows (Array.range 1 (Array.length rows))
  where
    rows = lines text
    slurpLine :: String -> Int -> Array ((Int /\ Int) /\ Char)
    slurpLine line rowNum =
      Array.zipWith
        (\char colNum -> ((colNum /\ rowNum) /\ char))
        (String.toCharArray line)
        (Array.range 1 (String.length line))

data Direction
  = North
  | South
  | East
  | West

derive instance eqDirection :: Eq Direction

burnFuse :: World -> String
burnFuse world =
  tailRec go { world, direction: South, position: ( 148 /\ 1 ), collected: "" }
  where
    go state =
      case Map.lookup state.position state.world of
        Nothing -> Done state.collected
        Just char ->
          let newCollected = state.collected <> String.singleton char
          in
            case step state.position state.world state.direction, neighbours state of
              Just nextPosition, _ ->
                Loop $ state { collected = newCollected
                             , position = nextPosition
                             }
              Nothing, [(newDirection /\ onlyPosition)] ->
                Loop $ state { collected = newCollected
                             , position = onlyPosition
                             , direction = newDirection
                             }
              Nothing, _ -> Done newCollected

    step position world direction =
      let (x /\ y) = position
          (dx /\ dy) = toDelta direction
      in validate (x + dx /\ y + dy) world

    validate position world =
      if Map.member position world
      then Just position
      else Nothing

    neighbours state =
      [ North, South, East, West ]
      # Array.filter ((/=) (opposite state.direction))
      # map (\direction -> map (Tuple direction) (step state.position state.world direction))
      # Array.catMaybes

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

toDelta :: Direction -> Int /\ Int
toDelta North = (0 /\ (-1))
toDelta South = (0 /\ 1)
toDelta West = ((-1) /\ 0)
toDelta East = (1 /\ 0)

solution1 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    String
solution1 = do
  String.fromCharArray <<< Array.filter interestingChar <<< String.toCharArray <<< burnFuse <$> readInput
  where
    interestingChar '+' = false
    interestingChar '|' = false
    interestingChar '-' = false
    interestingChar _ = true


solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    Int
solution2 = do
  String.length <<< burnFuse <$> readInput
