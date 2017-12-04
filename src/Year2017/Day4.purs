module Year2017.Day4 where

import Prelude

import Control.Fold (Fold, foldl, unfoldFold)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filter, length)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..), isNothing)
import Data.Set as Set
import Node.FS (FS)
import ParserUtils (parseFile)
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (alphaNum, char)

readInput :: forall eff. Eff (fs :: FS, exception :: EXCEPTION | eff) (Either ParseError (Array (Array (Array Char))))
readInput = parseFile lineParser "src/Year2017/Day4.txt"

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

solution1 :: Eff (fs :: FS, exception :: EXCEPTION) (Either ParseError Int)
solution1 = rmap solve <$> readInput
  where
    solve :: forall a. Ord a => Array (Array a) -> Int
    solve = length <<< filter not <<< map (foldl containsDuplicates)

solution2 :: Eff (fs :: FS, exception :: EXCEPTION) (Either ParseError Int)
solution2 = rmap solve <$> readInput
  where
    solve :: forall a. Ord a => Array (Array (Array a)) -> Int
    solve = length <<< filter not <<< map (foldl containsDuplicates <<< map Array.sort)
