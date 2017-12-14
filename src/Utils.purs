module Utils where

import Data.Set as Set
import Data.Set (Set)
import Data.Array as Array
import Prelude

inc :: Int -> Int
inc n = n + 1

dec :: Int -> Int
dec n = n - 1

-- | Oddly, Data.Set.intersection is extremely slow.
fastIntersection :: forall a. Ord a => Set a -> Set a -> Set a
fastIntersection a b =
  Set.fromFoldable (Array.filter (flip Set.member b) (Array.fromFoldable a))
