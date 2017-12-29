module Utils where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set, findMin)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse_)
import Data.Unfoldable (unfoldr)

inc :: Int -> Int
inc n = n + 1

dec :: Int -> Int
dec n = n - 1

-- | Oddly, Data.Set.intersection is extremely slow.
fastIntersection :: forall a. Ord a => Set a -> Set a -> Set a
fastIntersection a b =
  Set.fromFoldable (Array.filter (flip Set.member b) (Array.fromFoldable a))


type Walker graph node =
  { neighbours :: graph -> node -> Set node
  , unvisitedFn :: graph -> Set node
  }

-- | Find and label the connected groups in a graph, given a function
-- | that can generator neighbours, and one that gives the unvisited
-- | set.
connectedGroups ::
  forall graph node.
  Ord node
  => Walker graph node
  -> graph
  -> Array { index :: node
           , groupNumber :: Int
           }
connectedGroups {neighbours, unvisitedFn} graph =
  unfoldr visitor { toVisit: Set.empty
                  , unvisited: unvisitedFn graph
                  , groupNumber: 0
                  }
  where
    visitor {toVisit, unvisited, groupNumber} =
      case findMin (fastIntersection toVisit unvisited) of
        Just visiting ->
             Just ({ index: visiting
                   , groupNumber
                   }
                   /\
                   { toVisit: toVisit
                                # Set.union (neighbours graph visiting)
                                # Set.delete visiting
                   , unvisited: Set.delete visiting unvisited
                   , groupNumber
                   })
        -- This group is exhausted. Start a search for the next.
        Nothing ->
          case findMin unvisited of
            Nothing -> Nothing
            Just nextGroupStart -> visitor { toVisit: Set.singleton nextGroupStart
                                           , unvisited
                                           , groupNumber: groupNumber + 1
                                           }

repeatN :: forall a. Int -> (a -> a) -> a -> a
repeatN n action state =
  tailRec go (n /\ state)
  where
    go (n /\ state)
      | n > 0 = Loop (dec n /\ action state)
      | otherwise = Done state

showWorld :: forall eff a. (a -> Char) -> Int -> Map (Int /\ Int) a -> Eff (console :: CONSOLE | eff) Unit
showWorld format size world =
  traverse_ log $ map showRow $ Array.range 0 (dec size)
  where
    showRow :: Int -> String
    showRow row = String.fromCharArray $ map showCol $ Array.range 0 (dec size)
      where
        showCol :: Int -> Char
        showCol col =
          case Map.lookup (col /\ row) world of
            Nothing -> ' '
            Just x -> format x
