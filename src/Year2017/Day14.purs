module Year2017.Day14 where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array (length)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int (fromStringAs, hexadecimal, odd)
import Data.Maybe (Maybe(..))
import Data.Set (Set, findMin)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Unfoldable (unfoldr)
import Utils (dec, inc, fastIntersection)
import Year2017.Day10 (knotHash)

input :: String
input = "hxtvlmkl"

inputs :: Array String
inputs =
  prefix <$> Array.range 0 127
  where prefix s = input <> "-" <> show s

hexStringToBits :: String -> Maybe (Array Boolean)
hexStringToBits str =
  map (Array.concat <<< map intToBools) $ traverse (fromStringAs hexadecimal <<< String.singleton) (String.toCharArray str)
  where
    intToBools :: Int -> Array Boolean
    intToBools n =
      odd <<< div n <$> [ 8, 4, 2, 1 ] -- Probably the easiest way to ensure padding.

hashes :: Array String
hashes = knotHash <$> inputs

bitField :: Maybe (Array (Array Boolean))
bitField = traverse hexStringToBits hashes

solution1 :: Maybe Int
solution1 =
  Array.length <<< Array.filter id <<< Array.concat <$> bitField

sparseBitmap :: Array (Array Boolean) -> Set (Int /\ Int)
sparseBitmap bitmap =
  Set.fromFoldable $
  map fst $
  Array.filter (snd >>> id) $
  Array.concat $
  Array.zipWith (\y row -> Array.zipWith (\x -> Tuple (x /\ y))
                             (Array.range 0 (length row))
                             row
            )
    (Array.range 0 (length bitmap))
    bitmap

-- Yes, this is Day 12 copy & pasta. Will I get time to generalise?
connectedGroups :: Set (Int /\ Int) -> Array { index :: (Int /\ Int), groupNumber :: Int }
connectedGroups graph =
  unfoldr visitor { toVisit: Set.empty
                  , unvisited: graph
                  , groupNumber: 0
                  }
  where
    visitor {toVisit, unvisited, groupNumber} =
      case findMin (fastIntersection toVisit unvisited) of
        Just visiting@(x /\ y) ->
          let possibleNeighbours = [ x /\ inc y
                                   , x /\ dec y
                                   , inc x /\ y
                                   , dec x /\ y
                                   ]
              neighbours = Set.fromFoldable $ Array.filter (flip Set.member graph) possibleNeighbours
          in Just ({ index: visiting
                   , groupNumber
                   }
                   /\
                   { toVisit: toVisit
                                # Set.union neighbours
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

solution2 :: Maybe Int
solution2 =
  maximum =<< map _.groupNumber <<< connectedGroups <<< sparseBitmap <$> bitField
