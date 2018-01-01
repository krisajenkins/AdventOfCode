module Data.DefaultMap
  ( DefaultMap
  , empty
  , lookup
  , fromFoldable
  , alter
  )
  where

import Prelude

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple)

newtype DefaultMap k v = DefaultMap
  { map :: Map k v
  , defaultValue :: v
  }

empty :: forall k v. v -> DefaultMap k v
empty defaultValue = DefaultMap { map: Map.empty, defaultValue }

lookup :: forall k v. Ord k => k -> DefaultMap k v -> v
lookup key (DefaultMap {map, defaultValue}) =
  fromMaybe defaultValue $ Map.lookup key map

alter :: forall k v. Ord k => (v -> Maybe v) -> k -> DefaultMap k v -> DefaultMap k v
alter f key (DefaultMap defaultMap@{map, defaultValue }) =
  DefaultMap $ defaultMap { map = Map.alter (f <<< fromMaybe defaultValue) key map }

fromFoldable :: forall f k v. Ord k => Foldable f => v -> f (Tuple k v) -> DefaultMap k v
fromFoldable defaultValue foldable =
  DefaultMap { map: Map.fromFoldable foldable
             , defaultValue
             }
