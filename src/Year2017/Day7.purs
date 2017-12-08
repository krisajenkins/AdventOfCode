module Year2017.Day7 where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Either (Either(Right, Left))
import Data.Foldable (sum)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', traversed, (^..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String (fromCharArray)
import Data.Symbol (SProxy(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Witherable (partitioned)
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Prelude hiding (between)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (between, many1, optionMaybe, sepBy)
import Text.Parsing.StringParser.String (alphaNum, char, skipSpaces, string)

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Map String (Int /\ Array String))
readInput =
  map Map.fromFoldable <$> parseFile lineParser "src/Year2017/Day7.txt"
    >>= mustSucceed

lineParser :: Parser (String /\ (Int /\ Array String))
lineParser = do
  name <- nameParser
  skipSpaces
  weight <- between (char '(') (char ')') integer
  children <- fromMaybe [] <$> optionMaybe childrenParser
  pure $ (name /\ (weight /\ children))

childrenParser :: Parser (Array String)
childrenParser = do
  _ <- string " -> "
  skipSpaces
  Array.fromFoldable <$> (nameParser `sepBy` string ", ")

nameParser :: Parser String
nameParser = fromCharArray <<< Array.fromFoldable <$> many1 alphaNum

------------------------------------------------------------

findRoots :: Map String (Int /\ (Array String)) -> Set String
findRoots graph = Set.difference parents children
  where
    parents = Set.fromFoldable $ Map.keys graph
    children = Set.fromFoldable $ Array.concat $ Array.fromFoldable $ map snd $ Map.values graph

solution1 :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (Set String)
solution1 = findRoots <$> readInput

------------------------------------------------------------

newtype Summary = Summary
  { total :: Int
  , myWeight :: Int
  , parent :: String
  }

derive instance genericSummary :: Generic Summary
derive instance newtypeSummary :: Newtype Summary _
derive instance eqSummary :: Eq Summary

instance showSummary :: Show Summary where
  show = gShow

_total :: Lens' Summary Int
_total = _Newtype <<< prop (SProxy :: SProxy "total")

-- | Actually this isn't the solution - it just summarises the
-- | unbalanced node's children and their weights. The easiest way to
-- | do the final step is just to look at the data manually!
solution2 :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (Array (Either (Array Summary) Summary))
solution2 = solve <$> readInput
  where
    solve graph = visit graph <$> Array.fromFoldable (findRoots graph)
    visit graph node =
      let (weight /\ children) = fromMaybe (0 /\ []) $ Map.lookup node graph
          childrenResults = visit graph <$> children
          split = partitioned childrenResults
      in case split.left of
           [] ->
             let childrensTotals = split.right ^.. traversed <<< _total
             in if Set.size (Set.fromFoldable childrensTotals) > 1
                  then Left split.right
                  else Right $ Summary { parent: node
                                       , myWeight: weight
                                       , total: weight + sum childrensTotals
                                       }
           xs -> Left $ Array.concat split.left
