module Data.AS3.AST.Prims (
  between_braces
, between_parens
, between_brackets
, semi
, ss
, comma
, plusfold
, csv
, dots
, sym
, symR
) where

import           Data.AS3.AST.Def
import           Data.AS3.AST.ThirdParty
import           Data.List (intersperse)
import           Text.Parsec

between_braces :: As3Parser a -> As3Parser a
between_braces = between
  (spaces *> string "{" <* spaces)
  (spaces *> string "}" <* spaces)

between_parens :: As3Parser a -> As3Parser a
between_parens = between
  (spaces *> string "(" <* spaces)
  (spaces *> string ")" <* spaces)

between_brackets :: As3Parser a -> As3Parser a
between_brackets = between
  (spaces *> string "[" <* spaces)
  (spaces *> string "]" <* spaces)

semi :: As3Parser Char
semi = tok $ char ';'

ss :: As3Parser String
ss = many $ char ' '

comma :: As3Parser Char
comma = tok $ char ','

plusfold :: As3Parser String -> String -> As3Parser String
plusfold acc str = acc `parserPlus` (try $ string str)

csv :: As3Parser a -> As3Parser [a]
csv a = a `sepBy` (ss *> char ',' <* ss)

dots :: [String] -> As3Parser String
dots = return . concat . intersperse "."

sym :: (Show a) => a -> As3Parser String
sym = string . show

symR :: (Show a) => a -> As3Parser a
symR a = sym a >> return a
