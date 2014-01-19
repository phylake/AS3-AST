module Data.AS3.AST.Grammar.Lexicon where

import           Control.Monad.State
import           Data.AS3.AST.Def
import           Data.AS3.AST.Prims
import           Data.AS3.AST.Show
import           Data.AS3.AST.ThirdParty
import           Text.Parsec
import           Util.Misc (t31)
import qualified Control.Applicative as A
import qualified Data.HashTable.IO as H

-- $7.6 Identifier Names and Identifiers

scoped_identifier :: As3Parser Expression
scoped_identifier = do
  ps <- get_scope
  --p$ "scoped_identifier " ++ show ps
  case ps of
    PS_Class -> class_id
    PS_UntypedIds -> untyped_id
    PS_TypedIds -> typed_id

scope_mods :: As3Parser [ScopeMod]
scope_mods = scope_mod `sepEndBy1` (many1 $ char ' ') <?> "scope modifiers"
  where
    scope_mod :: As3Parser ScopeMod
    scope_mod =
          try (symR Public)
      <|> try (symR Protected)
      <|> try (symR Private)
      <|> try (symR Final)
      <|> try (symR Override)
      <|>     (symR Static) <?> "scope modifier"

user_defined_type :: As3Parser String
user_defined_type = liftM2 (:) upper (many alphaNum)

extendable_type :: As3Parser String
extendable_type = user_defined_type

implementable_type :: As3Parser String
implementable_type = user_defined_type

as3_type :: As3Parser Type
as3_type =
      try (symR T_int)
  <|> try (symR T_uint)
  <|> try (symR T_void)
  <|> try (symR T_undefined)
  <|> try (symR T_Number)
  <|> try (symR T_Boolean)
  <|> try (symR T_String)
  <|> try (symR T_Array)
  <|> try (symR T_Object)
  <|> try (string "Vector.<" *> as3_type <* string ">" >>= return . T_Vector)
  <|>     (liftM T_UserDefined user_defined_type)
  <?> "type id"

-- ^ identifiers can not start with a numeral
var_id :: As3Parser String
var_id = notFollowedBy reserved_word >> liftM2 (++) beg end
  where
    beg = many1 $ letter   <|> char '_'
    end = many  $ alphaNum <|> char '_'

cv :: As3Parser CV
cv = (string "var " >> return Var) <|> (string "const " >> return Const)

type_declaration :: As3Parser Type
type_declaration = ss *> char ':' *> ss *> as3_type

class_id :: As3Parser Expression
class_id = liftM4 ClassId scope_mods cv var_id type_declaration

function_body_id :: As3Parser Expression
function_body_id = fail "function_body_id" >> liftM3 FnId cv var_id type_declaration

-- ^ overlaps with variable_statement but as a special-case expression
typed_id :: As3Parser Expression
typed_id = liftM2 TypedId var_id type_declaration

untyped_id :: As3Parser Expression
untyped_id = liftM UntypedId var_id












line_terminator :: As3Parser String
line_terminator = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\r"
  <|> string "\n"

{-identifier :: As3Parser String
identifier =
      comments
  <|> line_terminator
  <|> as3Token-}

comments :: As3Parser String
comments = undefined

as3Token :: As3Parser String
as3Token =
      punctuator
  <|> numeric_literal
  <|> string_literal

reserved_word :: As3Parser String
reserved_word =
      try keyword
  <|> try null_literal
  <|> boolean_literal

keyword :: As3Parser String
keyword =
      try (string "break")
  <|> try (string "do")
 -- <|> try (string "instanceof")
 -- <|> try (string "typeof")
  <|> try (string "case")
  <|> try (string "else")
  <|> try (string "new")
  <|> try (string "var")
  <|> try (string "catch")
  <|> try (string "finally")
  <|> try (string "return")
  <|> try (string "void")
  <|> try (string "continue")
  <|> try (string "for")
  <|> try (string "for each")
  <|> try (string "switch")
  <|> try (string "while")
  <|> try (string "debugger")
  <|> try (string "function")
  <|> try (string "this")
  <|> try (string "with")
  <|> try (string "default")
  <|> try (string "if")
  <|> try (string "throw")
  <|> try (string "delete")
  <|> try (string "in")
  <|> try (string "try")
  <|> try (string "class")
 -- <|> try (string "enum")
  <|> try (string "extends")
  <|> try (string "super")
  <|> try (string "const")
 -- <|> try (string "export")
 -- <|> try (string "import")
  <|> try (string "implements")
 -- <|> try (string "let")
  <|> try (string "private")
  <|> try (string "public")
 -- <|> try (string "yield")
  <|> try (string "interface")
  <|> try (string "package")
  <|> try (string "protected")
  <|> try (string "static")
  <?> "keyword"

punctuator :: As3Parser String
punctuator =
      string "{"
  <|> string "}"
  <|> string "("
  <|> string ")"
  <|> string "["
  <|> string "]"
  <|> string "."
  <|> string ";"
  <|> string ","
  <|> string "<"
  <|> string ">"
  <|> string "<="
  <|> string ">="
  <|> string "=="
  <|> string "!="
  <|> string "==="
  <|> string "!=="
  <|> string "+"
  <|> string "-"
  <|> string "*"
  <|> string "%"
  <|> string "++"
  <|> string "--"
  <|> string "<<"
  <|> string ">>"
  <|> string ">>>"
  <|> string "&"
  <|> string "|"
  <|> string "^"
  <|> string "!"
  <|> string "~"
  <|> string "&&"
  <|> string "||"
  <|> string "?"
  <|> string ":"
  <|> string "="
  <|> string "+="
  <|> string "-="
  <|> string "*="
  <|> string "%="
  <|> string "<<="
  <|> string ">>="
  <|> string ">>>="
  <|> string "&="
  <|> string "|="
  <|> string "^="
  <?> "punctuator"

div_punctuator :: As3Parser String
div_punctuator = string "/" <|> string "/="

-- $7.8 Literals

literal :: As3Parser String
literal =
      null_literal
  <|> boolean_literal
  <|> numeric_literal
  <|> string_literal
  {-<|> regex_literal-}
  <?> "literal"

null_literal :: As3Parser String
null_literal = string "null"

boolean_literal :: As3Parser String
boolean_literal = string "true" <|> string "false" <?> "boolean literal"

numeric_literal :: As3Parser String
numeric_literal = try hex_integer_literal <|> decimal_literal <?> "numeric literal"

decimal_literal :: As3Parser String
decimal_literal =
      try (decimal_integer_literal >> string "." >> decimal_digits)
  <|> try (char '.' >> decimal_digits)
  <|> decimal_integer_literal

decimal_integer_literal :: As3Parser String
decimal_integer_literal =
      string "0"
  <|> (liftM2 (++) non_zero_digits (option "" decimal_digits))

decimal_digits :: As3Parser String
decimal_digits = many1 decimal_digit

decimal_digit :: As3Parser Char
decimal_digit = char '0' <|> non_zero_digit

non_zero_digits :: As3Parser String
non_zero_digits = many1 non_zero_digit

non_zero_digit :: As3Parser Char
non_zero_digit =
      char '1'
  <|> char '2'
  <|> char '3'
  <|> char '4'
  <|> char '5'
  <|> char '6'
  <|> char '7'
  <|> char '8'
  <|> char '9'

hex_integer_literal :: As3Parser String
hex_integer_literal = do
  char '0'
  char 'x' <|> char 'X'
  many1 hex_digit

hex_digit :: As3Parser Char
hex_digit =
      decimal_digit
  <|> char 'a'
  <|> char 'b'
  <|> char 'c'
  <|> char 'd'
  <|> char 'e'
  <|> char 'f'
  <|> char 'A'
  <|> char 'B'
  <|> char 'C'
  <|> char 'D'
  <|> char 'E'
  <|> char 'F'

string_literal :: As3Parser String
string_literal = do
  q <- quote -- ' or "
  str <- many $ satisfy (/= q)
  quote
  return $"\"" ++ str ++ "\""

quote :: As3Parser Char
quote = char '\'' <|> char '\"'

-- $7.8.5 Regular Expressions

regex_literal :: As3Parser String
regex_literal = undefined
