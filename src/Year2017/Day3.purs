module Year2017.Day3 where

import Prelude

import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Data.Foldable (sum)
import Data.Int (floor, pow, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Math (sqrt)

type Position = Tuple Int Int

positionOf :: Int -> Position
positionOf n =
  let target = n - 1
      root = floor $ sqrt $ toNumber target
      rootSequence = div root 2
      indexValue = pow root 2
      isEven = (mod root 2 == 0)
      offset = target - indexValue
      direction =
        if isEven
        then 1
        else -1
      startX /\ startY =
        if isEven
        then -rootSequence /\ -rootSequence
        else rootSequence + 1 /\ rootSequence
      dy = min offset root
      dx = offset - dy
  in
     (startX + (dx * direction))
     /\
     (startY + (dy * direction))

solution1 :: Position
solution1 = positionOf 289326

type State =
  { step :: Int
  , target :: Int
  , written :: Map Position Int
  }

neighboursOf :: Position -> Array Position
neighboursOf (x /\ y) = do
  dx <- [ -1, 0, 1 ]
  dy <- [ -1, 0, 1 ]
  pure $ (x + dx) /\ (y + dy)

firstLarger :: Int -> Int
firstLarger n =
  tailRec go { step: 1
             , target: n
             , written: Map.singleton (0 /\ 0) 1
             }

go :: State -> Step State Int
go { step, target, written } =
    let position = positionOf step
        neighbours = neighboursOf position
        neighbourSum :: Int
        neighbourSum = sum (fromMaybe 0 <<< flip Map.lookup written <$> neighbours)
    in if neighbourSum >= target
       then Done neighbourSum
       else Loop { step: step + 1
                 , target
                 , written: Map.insert position neighbourSum written
                 }

solution2 :: Int
solution2 = firstLarger 289326
