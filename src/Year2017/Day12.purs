module Year2017.Day12 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Utils (Walker, connectedGroups)
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.String (string)

type Graph = Map Int (Set Int)

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    Graph
readInput =
  Map.fromFoldable <$> (parseFile lineParser "src/Year2017/Day12.txt" >>= mustSucceed)

lineParser :: Parser (Int /\ Set Int)
lineParser = do
  from <- integer
  _ <- string " <-> "
  to <- Set.fromFoldable <$> integer `sepBy` string ", "
  pure $ from /\ to

walker :: Walker (Map Int (Set Int)) Int
walker =
  { neighbours: \graph visiting -> fromMaybe Set.empty $ Map.lookup visiting graph
  , unvisitedFn: Set.fromFoldable <<< Map.keys
  }

solution1 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    Int
solution1 = do
  input <- readInput
  pure $ Array.length $ Array.filter (_.groupNumber >>> eq 1) $ connectedGroups walker $ input

solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Maybe Int)
solution2 = do
  input <- readInput
  pure $ maximum $ map _.groupNumber $ connectedGroups walker input
