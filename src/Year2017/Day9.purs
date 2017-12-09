module Year2017.Day9 where

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr, sum)
import Data.Generic (class Generic, gShow)
import Data.String as String
import Data.Traversable (class Traversable, sequenceDefault, traverseDefault)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import ParserUtils (mustSucceed)
import Prelude hiding (between)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (between, many, sepBy)
import Text.Parsing.StringParser.String (anyChar, char, noneOf)

data Tree a
  = Node (Array (Tree a))
  | Leaf a

derive instance genericTree :: Generic a => Generic (Tree a)
derive instance eqTree :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map f (Leaf x) = Leaf (f x)
  map f (Node xs) = Node (map f <$> xs)

instance foldableTree :: Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node xs) = fold (foldMap f <$> xs)
  foldr f b (Leaf a) = f a b
  foldr f b (Node xs) = foldr (flip (foldr f)) b xs
  foldl f b (Leaf a) = f b a
  foldl f b (Node xs) = foldl (foldl f) b xs

instance traversable :: Traversable Tree where
  traverse = traverseDefault
  sequence = sequenceDefault

instance showTree :: (Generic a, Show a) => Show (Tree a) where
  show = gShow

garbageChar :: Parser String
garbageChar =
  (char '!' *> anyChar *> pure "")
  <|>
  (String.singleton <$> noneOf ['>'])

garbage :: Parser String
garbage =
  String.joinWith "" <<< Array.fromFoldable
    <$> between (char '<') (char '>')
          (many garbageChar)

groupParser :: Parser (Tree String)
groupParser =
  fix $ \recur ->
   (Leaf <$> garbage)
   <|>
   between (char '{') (char '}')
      (Node <<< Array.fromFoldable
         <$> recur `sepBy` char ','
      )

------------------------------------------------------------

readInput :: forall eff. Eff (exception :: EXCEPTION, fs :: FS | eff) (Tree String)
readInput = do
  mustSucceed =<< runParser groupParser <$> readTextFile UTF8 "src/Year2017/Day9.txt"

solution1 :: forall eff. Eff (exception :: EXCEPTION, fs :: FS | eff) Int
solution1 = groupTotal 1 <$> readInput

groupTotal :: forall a. Int -> Tree a -> Int
groupTotal n (Leaf _) = 0
groupTotal n (Node leaves) = n + sum (groupTotal (n + 1) <$> leaves)

solution2 :: forall eff. Eff (exception :: EXCEPTION, fs :: FS | eff) Int
solution2 = garbageTotal <$> readInput

garbageTotal :: Tree String -> Int
garbageTotal = sum <<< map String.length
