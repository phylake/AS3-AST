module Data.AS3.AST.Grammar.Statements where

import           Control.Monad
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Expressions
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Prims
import           Data.AS3.AST.ThirdParty
import           Text.Parsec

-- $Package-level

package_id :: As3Parser String
package_id = many1 lower `sepBy1` char '.' >>= dots

package :: As3Parser Statement
package = do
  string "package" <* ss
  name <- optionMaybe package_id
  body <- between_braces $ many package_body
  return $ Package name body
  <?> "package"

package_body :: As3Parser Statement
package_body = try (tok imporT) <|> as3_class

imporT :: As3Parser Statement
imporT = liftM Import (string "import " *> package_id <* semi) <?> "import"

-- $Class-level

as3_class :: As3Parser Statement
as3_class = do
  scopes <- scope_mods
  tok $ string "class"
  name <- tok user_defined_type
  extends <- optionMaybe $ string "extends " *> extendable_type <* ss -- make sure "extends" is the first match in order to fail fast and return Nothing
  implements <- optionMaybe $ string "implements " *> csv implementable_type -- make sure "implements" is the first match in order to fail fast and return Nothing
  body <- with_scope PS_Class $ between_braces $ many $ tok (source_element <* optional semi)
  return $ Class scopes name extends implements body

source_element :: As3Parser Statement
source_element = try tstatement <|> function_declaration

function_declaration :: As3Parser Statement
function_declaration = do
  mods <- scope_mods <* string "function "
  mAccessor <- optionMaybe accessor
  name <- var_id
  params <- with_scope PS_TypedIds $ between_parens $ assignment_expression `sepBy` comma
  returnType <- type_declaration
  body <- with_scope PS_UntypedIds $ between_braces $ many tstatement
  return $ FnDec mods mAccessor name params returnType body
  where
    accessor :: As3Parser Accessor
    accessor = (string "get " >> return Get) <|> (string "set " >> return Set)

tstatement :: As3Parser Statement
tstatement = tok (statement <* optional semi)

statement :: As3Parser Statement
statement =
      try block_statement
  <|> try variable_statement
  <|> try constant_statement -- ^ ‡
  <|> try empty_statement
  <|> try if_statement
  <|> try iteration_statement
  <|> try continue_statement
  <|> try break_statement
  <|> try return_statement
  <|> try with_statement
  <|> try labelled_statement -- before expression_statement
  <|> try expression_statement
  <|> try switch_statement
  {-<|> try throw_statement
  <|>     try_statement-}
  <?> "statement"

block_statement :: As3Parser Statement
block_statement = liftM Block $ between_braces $ many tstatement

variable_statement :: As3Parser Statement
variable_statement =
  string "var " *> with_scope PS_TypedIds idents <* optional semi where
    idents = liftM Variable comma_expression

constant_statement :: As3Parser Statement
constant_statement =
  string "const " *> with_scope PS_TypedIds idents <* optional semi where
    idents = liftM Constant comma_expression

empty_statement :: As3Parser Statement
empty_statement = semi *> return EmptyS

expression_statement :: As3Parser Statement
expression_statement = do
  notFollowedBy $ char '{'
  notFollowedBy $ string "function"
  liftM ExpressionStmt expression <* optional semi

if_statement :: As3Parser Statement
if_statement = liftM3 If
  (string "if" *> between_parens expression)
  tstatement
  (many $ try elif <|> el) where
    elif :: As3Parser Statement
    elif = liftM2 ElseIf
      (string "else if" *> between_parens expression)
      tstatement
    
    el :: As3Parser Statement
    el = liftM Else (tok (string "else") *> tstatement)

iteration_statement :: As3Parser Statement
iteration_statement =
      try do_while
  <|> try while
  <|> try for
  <|> try for_in
  <|>     for_each
  <?> "iteration stmt"
  where
    do_while :: As3Parser Statement
    do_while = liftM2 DoWhile
                 (tok (string "do") *> tok statement)
                 (tok (string "while") *> between_parens expression)

    while :: As3Parser Statement
    while = liftM2 While
              (string "while" *> between_parens expression)
              (statement)

    for :: As3Parser Statement
    for = do
      tok (string "for") *> tok (char '(')
      ms <- optionMaybe $ tok varOrExp
      me1 <- optionMaybe expression <* optional semi
      me2 <- optionMaybe expression <* closeParen
      s <- statement
      return $ For ms me1 me2 s
      where
        varOrExp :: As3Parser Statement
        varOrExp = noin (try variable_statement <|> expression_statement)

    for_in :: As3Parser Statement
    for_in = forCommon "for" ForIn
    
    for_each :: As3Parser Statement
    for_each = forCommon "for each" ForEach
    
    forCommon :: String
              -> (Statement -> Expression -> Statement -> Statement)
              -> As3Parser Statement
    forCommon preamble ctor = do
      tok $ string preamble
      tok $ char '('
      s <- tok lhsOrExp
      tok $ string "in"
      e <- expression <* closeParen
      body <- statement
      return $ ctor s e body
      where
        lhsOrExp :: As3Parser Statement
        lhsOrExp = noin (try variable_statement <|> liftM ExpressionStmt lhs_expression)

    closeParen :: As3Parser String
    closeParen = spaces *> string ")" <* spaces

continue_statement :: As3Parser Statement
continue_statement = liftM Continue
                       (string "continue" *> ss *> optionMaybe expression)

break_statement :: As3Parser Statement
break_statement = liftM Break
                    (string "break" *> ss *> optionMaybe expression)

return_statement :: As3Parser Statement
return_statement = liftM Return
                     (string "return" *> ss *> optionMaybe expression)

with_statement :: As3Parser Statement
with_statement = liftM2 With
                  (string "with" *> between_parens expression)
                  (statement)

switch_statement :: As3Parser Statement
switch_statement = do
  string "switch"
  e <- between_parens expression
  ss <- between_braces $ many (case_statement <|> default_statement)
  return $ Switch e ss
  where
    case_statement :: As3Parser SwitchBody
    case_statement = do
      string "case "
      e <- expression
      tok $ char ':'
      ss <- many tstatement
      return $ Case e ss

    default_statement :: As3Parser SwitchBody
    default_statement = do
      tok $ string "default:"
      liftM Default $ many tstatement

labelled_statement :: As3Parser Statement
labelled_statement = do
  ident <- var_id
  tok $ char ':'
  s <- tstatement
  return $ Labeled ident s

throw_statement :: As3Parser Statement
throw_statement = undefined

try_statement :: As3Parser Statement
try_statement = undefined

