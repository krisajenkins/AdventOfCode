module Year2017.Day6 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Control.Fold (Fold)
import Control.Fold as Fold
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (zipWith)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

offsetList :: forall a. { items :: Int, length :: Int, index :: Int |  a } -> Array Int
offsetList { items, length, index } =
  Array.drop (length - (index + 1)) basicList <> Array.take (length - (index + 1)) basicList
  where
    n = div items length
    remainder = mod items length
    basicList =
      Array.modifyAtIndices [length - 1] (\x -> x - items) $
        Array.replicate (remainder) (n + 1)
        <>
        Array.replicate (length - remainder) n

redistribute :: Array Int -> Array Int
redistribute xs =
  case Fold.foldl firstLargest xs of
    Just { item, itemIndex, index } ->
      let offsets = offsetList { length: index + 1 , index: itemIndex, items: item }
      in zipWith (+) xs offsets
    Nothing -> xs

firstLargest :: forall a. Ord a => Fold a (Maybe {itemIndex :: Int , item :: a , index :: Int})
firstLargest = Fold.unfoldFold_ Nothing reducer
  where
    reducer Nothing x = Just { index: 0, item: x, itemIndex: 0 }
    reducer (Just {index, item, itemIndex}) x =
      Just $ if x > item
               then { index: index + 1, item: x, itemIndex: index + 1 }
               else { index: index + 1, item, itemIndex }

type State = { steps :: Int, current :: Array Int, seen :: Map (Array Int) Int }

solve :: Array Int -> (State /\ Int)
solve input = tailRec go { steps: 0, current: input, seen: Map.empty }
  where
    go :: State -> Step State (State /\ Int)
    go {steps: oldSteps, current, seen: oldSeen} =
      let steps = oldSteps + 1
          next = redistribute current
          newState = {steps, current: next, seen: Map.insert next steps oldSeen }
      in case Map.lookup next oldSeen of
           Nothing -> Loop newState
           Just n -> Done (newState /\ (steps - n))

input :: Array Int
input = [ 11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11 ]

solution1 :: Int
solution1 =
  let ({ steps } /\ _) = solve input
  in steps

solution2 :: Int
solution2 =
  let (_ /\ cycleAfter) = solve input
  in cycleAfter
