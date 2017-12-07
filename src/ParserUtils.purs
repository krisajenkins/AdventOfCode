module ParserUtils where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.String (trim)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepBy)
import Text.Parsing.StringParser.String (char, string)

digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9

integer :: Parser Int
integer = foldl (\acc n -> (acc * 10) + n) 0 <$> many1 digit

parseFile ::
  forall eff a.
  Parser a
  -> String
  -> Eff
       (fs :: FS, exception :: EXCEPTION | eff)
       (Either ParseError (Array a))
parseFile lineParser filename =
  runParser (fileParser lineParser) <<< trim <$> readTextFile UTF8 filename

mustSucceed :: forall eff e a. Show e => Either e a -> Eff (exception :: EXCEPTION | eff) a
mustSucceed (Right v) = pure v
mustSucceed (Left err) = throwException $ error $ show err

fileParser :: forall a. Parser a -> Parser (Array a)
fileParser lineParser = Array.fromFoldable <$> lineParser `sepBy` char '\n'
