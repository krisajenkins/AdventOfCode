module Year2016.Day1 where

import Prelude
import Utils (inc,dec)
import Control.Alternative ((<|>))
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', _Right, over, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import ParserUtils (integer)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many, optional)
import Text.Parsing.StringParser.String (char)

------------------------------------------------------------

data Direction = North | South | East | West

derive instance genericDirection :: Generic Direction
instance showDirection :: Show Direction where
  show = gShow

data Cmd = TurnLeft | TurnRight | Move Int

derive instance genericCmd :: Generic Cmd
instance showCmd :: Show Cmd where
  show = gShow

newtype Point =
  Point { x :: Int
        , y :: Int
        }
derive instance genericPoint :: Generic Point
derive instance ordPoint :: Ord Point
derive instance eqPoint :: Eq Point
derive instance newtypePoint :: Newtype Point _
instance showPoint :: Show Point where
  show = gShow

newtype State =
  State { point :: Point
        , facing :: Direction
        , visited :: Set Point
        }
derive instance newtypeState :: Newtype State _
instance showState :: Show State where
  show = show <<< view _visited

_x :: Lens' Point Int
_x = _Newtype <<< prop (SProxy :: SProxy "x")

_y :: Lens' Point Int
_y = _Newtype <<< prop (SProxy :: SProxy "y")

_visited :: Lens' State (Set Point)
_visited = _Newtype <<< prop (SProxy :: SProxy "visited")

_point :: Lens' State Point
_point = _Newtype <<< prop (SProxy :: SProxy "point")

_facing :: Lens' State Direction
_facing = _Newtype <<< prop (SProxy :: SProxy "facing")

------------------------------------------------------------

cmdParser :: Parser Cmd
cmdParser = (char 'L' $> TurnLeft)
        <|> (char 'R' $> TurnRight)
        <|> Move <$> integer

cmdsParser :: Parser (List Cmd)
cmdsParser = many (cmdParser <* (optional (many (char ',' <|> char ' '))))

------------------------------------------------------------

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight = turnLeft <<< turnLeft <<< turnLeft

calculate :: Either Point State -> Cmd -> Either Point State
calculate (Left point) _ = Left point
calculate state TurnLeft = over (_Right <<< _facing) turnLeft state
calculate state TurnRight = over (_Right <<< _facing) turnRight state
calculate state (Move 0) = state
calculate (Right state) (Move n) =
  let nextState =
        case view _facing state of
          West  -> over (_point <<< _x) dec state
          East  -> over (_point <<< _x) inc state
          North -> over (_point <<< _y) inc state
          South -> over (_point <<< _y) dec state
      nextPoint = view _point nextState
  in if Set.member nextPoint (view _visited state)
     then Left nextPoint
     else calculate
            (Right (over _visited (Set.insert nextPoint) nextState))
            (Move (n - 1))

------------------------------------------------------------

initialState :: State
initialState =
  State { facing: North
        , point: Point { x: 0, y: 0 }
        , visited: Set.empty
        }

input :: String
input = "L4, R2, R4, L5, L3, L1, R4, R5, R1, R3, L3, L2, L2, R5, R1, L1, L2, R2, R2, L5, R5, R5, L2, R1, R2, L2, L4, L1, R5, R2, R1, R1, L2, L3, R2, L5, L186, L5, L3, R3, L5, R4, R2, L5, R1, R4, L1, L3, R3, R1, L1, R4, R2, L1, L4, R5, L1, R50, L4, R3, R78, R4, R2, L4, R3, L4, R4, L1, R5, L4, R1, L2, R3, L2, R5, R5, L4, L1, L2, R185, L5, R2, R1, L3, R4, L5, R2, R4, L3, R4, L2, L5, R1, R2, L2, L1, L2, R2, L2, R1, L5, L3, L4, L3, L4, L2, L5, L5, R2, L3, L4, R4, R4, R5, L4, L2, R4, L5, R3, R1, L1, R3, L2, R2, R1, R5, L4, R5, L3, R2, R3, R1, R4, L4, R1, R3, L5, L1, L3, R2, R1, R4, L4, R3, L3, R3, R2, L3, L3, R4, L2, R4, L3, L4, R5, R1, L1, R5, R3, R1, R3, R4, L1, R4, R3, R1, L5, L5, L4, R4, R3, L2, R1, R5, L3, R4, R5, L4, L5, R2"

parsedCmds :: Either ParseError (List Cmd)
parsedCmds = runParser cmdsParser input

runCmds :: List Cmd -> Either Point State
runCmds = foldl calculate (Right initialState)

main :: Either ParseError (Either Point State)
main =  rmap runCmds parsedCmds
