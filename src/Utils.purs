module Utils where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set, findMin)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
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
