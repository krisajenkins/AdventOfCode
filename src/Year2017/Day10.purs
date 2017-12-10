module Year2017.Day10 where

import Prelude

import Control.Monad.State (class MonadState, execState)
import Data.Array as Array
import Data.Foldable (product)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', modifying, use, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Utils (inc)

newtype Rope a = Rope
  { items :: Array a
  , skip :: Int
  , distanceTravelled :: Int
  }

derive instance genericRope :: Generic a => Generic (Rope a)
derive instance eqRope :: Eq a => Eq (Rope a)
derive instance newtypeRope :: Newtype (Rope a) _

instance showRope :: (Generic a, Show a) => Show (Rope a) where
  show = gShow

_items :: forall a. Lens' (Rope a) (Array a)
_items = _Newtype <<< prop (SProxy :: SProxy "items")

_distanceTravelled :: forall a. Lens' (Rope a) Int
_distanceTravelled = _Newtype <<< prop (SProxy :: SProxy "distanceTravelled")

_skip :: forall a. Lens' (Rope a) Int
_skip = _Newtype <<< prop (SProxy :: SProxy "skip")

twist :: forall a. Int -> Array a -> Array a
twist n xs =
  Array.drop n xs
  <>
  (Array.reverse (Array.take n xs))

cycle :: forall a. Int -> Array a -> Array a
cycle n xs =
  Array.drop delta xs <> Array.take delta xs
  where
    delta = mod n (Array.length xs)

step :: forall m a. MonadState (Rope a) m => Int -> m Int
step n = do
  skip <- use _skip
  modifying _items (twist n)
  modifying _items (cycle skip)
  modifying _distanceTravelled ((+) (n + skip))
  modifying _skip inc
  use _distanceTravelled

runHash :: forall t. Traversable t => Array Int -> t Int -> Array Int
runHash xs lengths =
  let initialState = Rope { skip: 0, distanceTravelled: 0, items: xs }
      finalState = execState
                      (traverse step lengths)
                      initialState
      len = Array.length xs
      delta = len - (mod (finalState ^. _distanceTravelled) len)
  in cycle delta (finalState ^. _items)
------------------------------------------------------------
input :: Array Int
input = [94, 84, 0, 79, 2, 27, 81, 1, 123, 93, 218, 23, 103, 255, 254, 243]

solution1 :: Int
solution1 =
  product $ Array.take 2 $ runHash (Array.range 0 255) input
------------------------------------------------------------

inputString :: String
inputString = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

salt :: Array Int
salt = [17, 31, 73, 47, 23]

knotHash :: String -> String
knotHash str = str

solution2 :: String
solution2 = knotHash inputString
