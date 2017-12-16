module Year2017.Day17 where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Maybe (Maybe(Just))
import Utils (inc)

type State =
  { position :: Int
  , stepSize :: Int
  , value :: Int
  , limit :: Int
  , cells :: Maybe (Array Int)
  }

spinlockStep :: State -> Step State State
spinlockStep state@{ position, stepSize, value, limit, cells } =
  if value >= limit
  then Done state
  else let newValue = inc value
           newPosition = (mod (position + stepSize) newValue) + 1
           newCells = Array.insertAt newPosition newValue =<< cells
       in Loop $ state { position = newPosition
                       , value = newValue
                       , cells = newCells
                       }

spinlock :: forall r. { stepSize :: Int, limit :: Int | r } -> State
spinlock {stepSize, limit} =
  tailRec spinlockStep
    { position: 0
    , value: 0
    , cells: Just $ Array.singleton 0
    , limit
    , stepSize
    }

solution1 :: Maybe Int
solution1 = do
  let spun = spinlock { stepSize: 349, limit }
  cells <- spun.cells
  marker <- Array.elemIndex limit cells
  Array.index cells (inc marker)
  where
    limit = 2017

solution2 :: Int
solution2 = tailRec go { position: 0, value: 0, latest : 0, stepSize: 349 }
  where
    limit = 50 * 1000 * 1000
    go { position, value, latest, stepSize } =
      let newPosition = mod (position + stepSize + 1) value
          newValue = inc value
      in if value >= limit
         then Done latest
         else if newPosition == 0
                 then Loop { position: newPosition
                           , value: newValue
                           , latest: value
                           , stepSize
                           }
                 else Loop { position: newPosition
                           , value: newValue
                           , latest: latest
                           , stepSize
                           }
