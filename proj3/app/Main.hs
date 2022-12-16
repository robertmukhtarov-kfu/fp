module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

import Control.Monad
import Data.Void

type Parser = Parsec Void String

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\""

csvRes :: [[String]]
csvRes =
  [ [ "col1",  "col2",  "col3" ]
  , [ "r2 c1", "r2 c2", "r2 c3" ]
  , [ "r3,c1", "r3,c2", "r3,c3" ]
  ]

csvParser :: Parser [[String]]
csvParser = sepBy rowParser (char '\n')

rowParser :: Parser [String]
rowParser = sepBy (quotedCellParser <|> notQuotedCellParser) (char ',')

notQuotedCellParser :: Parser String
notQuotedCellParser = many (noneOf ",\n")

quotedCellParser :: Parser String
quotedCellParser = char '"' *> many (noneOf "\"") <* char '"'

main :: IO ()
main = parseTest csvParser csv