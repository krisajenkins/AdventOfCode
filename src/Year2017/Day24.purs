module Year2017.Day24 where
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (class Foldable, sum)
import Data.Function (on)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import ParserUtils (integer, mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (char)

readInput :: forall eff.
  Eff
    (fs :: FS, exception :: EXCEPTION | eff)
    (List (Tuple Int Int))
readInput =
  parseFile partParser "src/Year2017/Day24.txt"
    >>= mustSucceed

partParser :: Parser (Tuple Int Int)
partParser = do
  a <- integer
  _ <- char '/'
  b <- integer
  pure $ Tuple a b

asBigraph :: forall f a.
  Foldable f => Ord a => f (Tuple a a) -> Map a (Array {to :: a , id :: Int})
asBigraph =
  Array.fromFoldable
  >>> Array.mapWithIndex (\n (Tuple a b) -> [ Tuple a [{to: b, id: n}]
                                            , Tuple b [{to: a, id: n}]])
  >>> Array.concat
  >>> Map.fromFoldableWith (<>)

type Answer = { strength :: Int, path :: List (Array Int) }
findLargest ::
  (Answer -> Answer -> Ordering) ->
  Map Int (Array {to :: Int, id :: Int}) -> Set Int -> Int -> Answer
findLargest sortFn bigraph visited from =
  case Array.filter (_.id >>> flip Set.member visited >>> not) $ fromMaybe [] $ Map.lookup from bigraph of
    [] -> { strength: 0, path: Nil }
    xs ->
      let subpaths = map (\node -> node /\ findLargest sortFn
                                     bigraph
                                     (Set.insert node.id visited)
                                     node.to) xs
      in case Array.head (Array.sortBy (sortFn `on` snd) subpaths) of
           Nothing -> { strength: 0, path: Nil }
           Just (node /\ { strength: n, path: ps }) ->
             { strength: n + from + node.to
             , path: Cons [from, node.to] ps
             }


longestPath = compare `on` (negate <<< List.length <<< _.path)
highestStrength = compare `on` (negate <<< _.strength)

solution1 :: forall eff.
  Eff
    (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | eff)
    Int
solution1 = do
  input <- asBigraph <$> readInput
  let answer = findLargest
               highestStrength
               input Set.empty 0
  logShow answer.path
  logShow (answer.path # Array.fromFoldable # Array.concat # sum)
  pure answer.strength

solution2 :: forall eff.
  Eff
    (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | eff)
    Int
solution2 = do
  input <- asBigraph <$> readInput
  let answer = findLargest
               (longestPath <> highestStrength)
               input Set.empty 0
  logShow answer.path
  logShow (answer.path # Array.fromFoldable # Array.concat # sum)
  pure answer.strength
