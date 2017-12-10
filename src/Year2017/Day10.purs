module Year2017.Day10 where

import Prelude

import Control.Monad.Rec.Class (Step(Loop, Done), tailRec)
import Control.Monad.State (class MonadState, execState)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldl, product)
import Data.Generic (class Generic, gShow)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits (xor)
import Data.Lens (Lens', modifying, use, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Utils (dec, inc)

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
  Array.drop delta xs <> (Array.reverse (Array.take delta xs))
  where
    delta = mod n (Array.length xs)

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

initialRope :: forall a. Array a -> Rope a
initialRope xs = Rope { skip: 0, distanceTravelled: 0, items: xs }

finaliseRope :: forall a. Rope a -> Array a
finaliseRope finalState =
  let finalItems = finalState ^. _items
      len = Array.length finalItems
      delta = len - (mod (finalState ^. _distanceTravelled) len)
  in cycle delta finalItems

runHash :: forall t. Traversable t => Array Int -> t Int -> Array Int
runHash xs lengths =
  finaliseRope $ execState
                   (traverse step lengths)
                   (initialRope xs)

------------------------------------------------------------

initialItems :: Array Int
initialItems = Array.range 0 255

input :: Array Int
input = [94, 84, 0, 79, 2, 27, 81, 1, 123, 93, 218, 23, 103, 255, 254, 243]

solution1 :: Int
solution1 =
  product $ Array.take 2 $ runHash initialItems input

------------------------------------------------------------

xorBlock :: forall t. Foldable t => t Int -> Int
xorBlock = foldl xor 0

showHash :: forall t. Foldable t => t Int -> String
showHash = foldl (\a -> append a <<< padLeft "0" <<< toStringAs hexadecimal) ""
  where
    padLeft padding xs =
      case String.length xs of
        0 -> padding <> padding
        1 -> padding <> xs
        _ -> xs

knotHash :: String -> String
knotHash str = hash
  where
    lengths = toLengths str <> salt
    finalItems = finaliseRope $ tailRec go (64 /\ initialRope initialItems)
    go (0 /\ rope) = Done rope
    go (n /\ rope) = Loop (dec n /\ execState (traverse step lengths) rope)
    xors = Array.take 16 (chunk 16 finalItems)
    decs = xorBlock <$> xors
    hash = showHash decs

chunk :: Int -> Array Int -> Array (Array Int)
chunk _ [] = [[]]
chunk n xs =
  Array.cons
    (Array.take n xs)
    (chunk n (Array.drop n xs))

toLengths :: String -> Array Int
toLengths xs =
  toCharCode <$> String.toCharArray xs

salt :: Array Int
salt = [17, 31, 73, 47, 23]

------------------------------------------------------------

solution2 :: String
solution2 =
  knotHash inputString
  where
    inputString = String.joinWith "," $ show <$> input
