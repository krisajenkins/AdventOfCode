module ParserUtils where

import Control.Alternative ((<|>))
import Data.Foldable (foldl)
import Prelude
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (string)

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
