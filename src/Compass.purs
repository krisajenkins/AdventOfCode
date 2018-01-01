module Compass where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Utils (dec, inc)

data Direction = North | South | East | West

derive instance genericDirection :: Generic Direction
derive instance eqDirection :: Eq Direction
instance showDirection :: Show Direction where
  show = gShow

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight = turnLeft <<< turnLeft <<< turnLeft

data Cmd = TurnLeft | TurnRight | Move Int

derive instance genericCmd :: Generic Cmd
instance showCmd :: Show Cmd where
  show = gShow

newtype Avatar =
  Avatar { x :: Int
         , y :: Int
         , direction :: Direction
         }
derive instance genericAvatar :: Generic Avatar
derive instance eqAvatar :: Eq Avatar
derive instance newtypeAvatar :: Newtype Avatar _
instance showAvatar :: Show Avatar where
  show = gShow

_x :: Lens' Avatar Int
_x = _Newtype <<< prop (SProxy :: SProxy "x")

_y :: Lens' Avatar Int
_y = _Newtype <<< prop (SProxy :: SProxy "y")

_direction :: Lens' Avatar Direction
_direction = _Newtype <<< prop (SProxy :: SProxy "direction")

handleCmd :: Cmd -> Avatar -> Avatar
handleCmd TurnLeft = over _direction turnLeft
handleCmd TurnRight = over _direction turnRight
handleCmd (Move n) = do
  d <- view _direction
  case d of
    West  -> over _x dec
    East  -> over _x inc
    North -> over _y dec
    South -> over _y inc

handleCmds :: forall f. Foldable f => Avatar -> f Cmd -> Avatar
handleCmds = foldl (flip handleCmd)
