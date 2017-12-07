module Year2017.Day4 where

import Prelude

import Control.Fold (Fold, foldl, unfoldFold)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filter, length)
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.Set as Set
import Node.FS (FS)
import ParserUtils (mustSucceed, parseFile)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (alphaNum, char)

readInput :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (Array (Array (Array Char)))
readInput = mustSucceed =<< parseFile lineParser "src/Year2017/Day4.txt"

lineParser :: Parser (Array (Array Char))
lineParser = Array.fromFoldable <<< map Array.fromFoldable <$> many1 alphaNum `sepBy` char ' '

containsDuplicates :: forall a. Ord a => Fold a Boolean
containsDuplicates =
  unfoldFold
    (Just Set.empty)
    reducer
    isNothing
  where
    reducer Nothing _ = Nothing
    reducer (Just seen) new =
      if Set.member new seen
      then Nothing
      else (Just (Set.insert new seen))

solution1 :: Eff (fs :: FS, exception :: EXCEPTION) Int
solution1 = solve <$> readInput
  where
    solve :: forall a. Ord a => Array (Array a) -> Int
    solve = length <<< filter not <<< map (foldl containsDuplicates)

solution2 :: Eff (fs :: FS, exception :: EXCEPTION) Int
solution2 = solve <$> readInput
  where
    solve :: forall a. Ord a => Array (Array (Array a)) -> Int
    solve = length <<< filter not <<< map (foldl containsDuplicates <<< map Array.sort)
