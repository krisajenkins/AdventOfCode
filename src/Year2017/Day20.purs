module Year2017.Day20 where

import Prelude

import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Function (on)
import Data.Generic (class Generic, gShow)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.Tuple (Tuple(..), fst, snd)
import Math (sqrt)
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (char, string)

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List Particle)
readInput =
  parseFile particleParser "src/Year2017/Day20.txt"
    >>= mustSucceed

newtype Coord = Coord
  { x :: Int
  , y :: Int
  , z :: Int
  }

derive instance genericCoord :: Generic Coord
derive instance eqCoord :: Eq Coord
derive instance newtypeCoord :: Newtype Coord _
instance showCoord :: Show Coord where
  show = gShow

newtype Particle = Particle
  { position :: Coord
  , velocity :: Coord
  , acceleration :: Coord
  }

derive instance genericParticle :: Generic Particle
derive instance eqParticle :: Eq Particle
derive instance newtypeParticle :: Newtype Particle _
instance showParticle :: Show Particle where
  show = gShow

particleParser :: Parser Particle
particleParser = do
  position <- string "p=" *> coordParser
  _ <- string ", "
  velocity <- string "v=" *> coordParser
  _ <- string ", "
  acceleration <- string "a=" *> coordParser
  pure $ Particle { position, velocity, acceleration }

coordParser :: Parser Coord
coordParser = do
  _ <- char '<'
  x <- integer
  _ <- char ','
  y <- integer
  _ <- char ','
  z <- integer
  _ <- char '>'
  pure $ Coord {x,y,z}

------------------------------------------------------------

distance :: Coord -> Int
distance (Coord {x,y,z}) =
  abs x + abs y + abs z

ultimateCloseness :: Tuple Int Particle -> Tuple Int Particle -> Ordering
ultimateCloseness =
  (compare `on` (snd >>> unwrap >>> _.acceleration >>> distance))
  `append`
  (compare `on` (snd >>> unwrap >>> _.velocity >>> distance))
  `append`
  (compare `on` (snd >>> unwrap >>> _.position >>> distance))

solution1 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Maybe Int)
solution1 = do
    map fst
    <<< List.head
    <<< List.sortBy ultimateCloseness
    <<< List.mapWithIndex Tuple
    <$> readInput

------------------------------------------------------------

type Time = Number

data IntersectionTime
  = Never | Always | Once Time | Twice Time Time

derive instance genericIntersectionTime :: Generic IntersectionTime
derive instance eqIntersectionTime :: Eq IntersectionTime
instance showIntersectionTime :: Show IntersectionTime where
  show = gShow

instance semigroupIntersectionTime :: Semigroup IntersectionTime where
  append Never _ = Never
  append _ Never = Never
  append Always b = b
  append a Always = a
  append (Once a) (Once b)
    | a == b = Once a
    | otherwise = Never
  append (Once a) (Twice b c)
    | a == b || a == c = Once a
    | otherwise = Never
  append (Twice a b) (Once c)
    | a == c || b == c = Once c
    | otherwise = Never
  append (Twice a b) (Twice c d)
    | a == c || a == d = Once a
    | b == c || b == d = Once b
    | otherwise = Never

hasIntersection :: Particle -> Particle -> Boolean
hasIntersection p1 p2 =
  findIntersection p1 p2 /= Never

findIntersection :: Particle -> Particle -> IntersectionTime
findIntersection p1 p2 =
  Array.foldl append Always $ findIntersections p1 p2

findIntersections :: Particle -> Particle -> Array IntersectionTime
findIntersections p1 p2 =
  [ solveForDimension (unwrap >>> _.x) p1 p2
  , solveForDimension (unwrap >>> _.y) p1 p2
  , solveForDimension (unwrap >>> _.z) p1 p2
  ]

-- | Let's construct and solve a quadratic equation!
solveForDimension :: (Coord -> Int) -> Particle -> Particle -> IntersectionTime
solveForDimension dimension (Particle p1) (Particle p2) =
  let x = toNumber $ ((_.acceleration >>> dimension) p1) - ((_.acceleration >>> dimension) p2)
      v = toNumber $ ((_.velocity >>> dimension) p1) - ((_.velocity >>> dimension) p2)
      p = toNumber $ ((_.position >>> dimension) p1) - ((_.position >>> dimension) p2)
      -- (0.5xt^2) + (0.5xvt) + p
      a = 0.5 * x
      b = (v + (0.5 * x))
      c = p
  in case a, b, c of
       -- Stationary, same point.
       0.0, 0.0, 0.0 ->
         Always
       -- Stationary, different point.
       0.0, 0.0, _ ->
         Never
       -- Linear equation.
       0.0, _, _->
         (Once (-c / b))
       -- Quadratic equation.
       _, _, _ ->
         let discriminant = ((b * b) - (4.0 * a * c))
         in if discriminant < 0.0
            then Never
            else let rootTerm = sqrt discriminant
                     divisor = 2.0 * a
                     left = ((-b) + rootTerm) / divisor
                     right = ((-b) - rootTerm) / divisor
                 in (Twice left right)

findSurvivors :: List Particle -> List Particle
findSurvivors Nil = Nil
findSurvivors (Cons p ps) =
  case List.partition (hasIntersection p) ps of
    { yes: Nil } -> Cons p (findSurvivors ps)
    { no: xs } -> findSurvivors xs

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    Int
solution2 =
  List.length <<< findSurvivors <$> readInput
