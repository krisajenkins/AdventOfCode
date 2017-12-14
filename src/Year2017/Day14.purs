module Year2017.Day14 where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array (length)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int (fromStringAs, hexadecimal, odd)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Utils (Walker, connectedGroups, dec, inc)
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

walker :: Walker (Set (Tuple Int Int)) (Tuple Int Int)
walker =
  { neighbours: \graph (x /\ y) ->
                   Set.fromFoldable
                   $ Array.filter (flip Set.member graph)
                   $ [ x /\ inc y
                     , x /\ dec y
                     , inc x /\ y
                     , dec x /\ y
                     ]
  , unvisitedFn: id
  }

solution2 :: Maybe Int
solution2 =
  maximum =<< map _.groupNumber <<< connectedGroups walker <<< sparseBitmap <$> bitField
