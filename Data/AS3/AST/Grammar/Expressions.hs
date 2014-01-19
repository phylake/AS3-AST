module Data.AS3.AST.Grammar.Expressions where

import           Control.Monad
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Prims
import           Data.AS3.AST.ThirdParty
import           Text.Parsec

-- $11.1 Primary Expressions

primary_expression :: As3Parser Expression
primary_expression =
      ((try $ string "this") >> return This)
  <|> try paren_group
  <|> try (liftM TODO_E literal)
  <|> try array_literal
  <|> try object_literal
  <|> try scoped_identifier -- check last because it fails slower
  <?> "primary_expression"

paren_group :: As3Parser Expression
paren_group = liftM ParenGroup $ between_parens comma_expression

array_literal :: As3Parser Expression
array_literal = liftM ArrayLiteral $ between_brackets comma_expression

element_list :: As3Parser Expression
element_list = undefined

object_literal :: As3Parser Expression
object_literal = liftM ObjectLiteral $ between_braces kvps where
  kvps :: As3Parser Expression
  kvps = liftM Comma $ property_assignment `sepBy` comma

  property_assignment :: As3Parser Expression
  property_assignment = liftM2 KeyValue
    (property_name <* ss <* char ':' <* ss)
    assignment_expression

  property_name :: As3Parser Expression
  property_name =
        try untyped_id
    <|> try (liftM (Lit . L_String) string_literal)
    <|> try (liftM (Lit . L_Number) numeric_literal)

-- $11.2 Left-Hand-Side Expressions

{-
MemberExpression:
    PrimaryExpression
    FunctionExpression
    MemberExpression [ Expression ]
    MemberExpression . IdentifierName
    new MemberExpression Arguments
-}
member_expression :: As3Parser Expression
member_expression = subexpressions >>= loop
  where
    subexpressions :: As3Parser Expression
    subexpressions = try primary_expression {-<|> function_expression-}
    
    loop :: Expression -> As3Parser Expression
    loop e =
          try (call loop e)
      <|> try (array_access loop e)
      <|> return e

function_expression :: As3Parser Expression
function_expression = undefined
{-function_expression = liftM3 FunctionExp
                        (optionMaybe identifier)
                        comma_expression
                        (many statement)-}

new_expression :: As3Parser Expression
new_expression =
      try (liftM New $ string "new " *> new_expression)
  <|> member_expression
  <?> "new expression"

{-
CallExpression:
    MemberExpression Arguments
    CallExpression Arguments
    CallExpression [ Expression ]
    CallExpression . IdentifierName
-}
call_expression :: As3Parser Expression
call_expression = subexpressions >>= loop
  where
    subexpressions :: As3Parser Expression
    subexpressions = liftM2 CallEMember member_expression arguments
    
    loop :: Expression -> As3Parser Expression
    loop e =
          try (call_arguments e)
      <|> try (call loop e)
      <|> try (array_access loop e)
      <|> return e

    call_arguments :: Expression -> As3Parser Expression
    call_arguments l = do
      r <- arguments
      loop $ CallEArguments l r

    arguments :: As3Parser Expression
    arguments = liftM Comma $
                between_parens $ assignment_expression `sepBy` comma

lhs_expression :: As3Parser Expression
lhs_expression = try call_expression <|> new_expression <?> "lhs_expression"

-- $11.3 Postfix Expressions

postfix_expression :: As3Parser Expression
postfix_expression =
      try (liftM2 Postfix (lhs_expression <* ss) unary_expression_post)
  <|> lhs_expression
  <?> "postfix_expression"
  where
    unary_expression_post :: As3Parser UnaryOp
    unary_expression_post =
          symR Increment
      <|> symR Decrement
      <?> "unary POSTFIX op"

-- $11.4 Unary Operators

unary_expression :: As3Parser Expression
unary_expression =
      try (liftM2 Unary unary_expression_pre unary_expression)
  <|> postfix_expression
  where
    unary_expression_pre :: As3Parser UnaryOp
    unary_expression_pre =
          (symR Delete)
      <|> (symR Void)
      <|> (symR TypeOf)
      <|> try (symR Increment)
      <|> try (symR Decrement)
      <|> (symR Positive)
      <|> (symR Negative)
      <|> (symR BitwiseNOT)
      <|> (symR LogicalNOT)
      <?> "unary PREFIX op"

-- $11.5 Multiplicative Operators

multiplicative_expression :: As3Parser Expression
multiplicative_expression =
  chainl1 (tok unary_expression) multiplicative_op
  <?> "multiplicative expression"
  where
    multiplicative_op :: As3Parser (Expression -> Expression -> Expression)
    multiplicative_op =
          linkL Multiplication
      <|> linkL Division
      <|> linkL Modulo
      <?> "multiplicative operator"

-- $11.6 Additive Operators

additive_expression :: As3Parser Expression
additive_expression =
  chainl1 (tok multiplicative_expression) additive_op
  <?> "additive expression"
  where
    additive_op :: As3Parser (Expression -> Expression -> Expression)
    additive_op =
          linkL Addition
      <|> linkL Subtraction
      <?> "additive operator"

-- $11.7 Bitwise Shift Operators

shift_expression :: As3Parser Expression
shift_expression =
  chainl1 (tok additive_expression) shift_op
  <?> "shift expression"
  where
    shift_op :: As3Parser (Expression -> Expression -> Expression)
    shift_op =
          linkL URShift -- >>> before >>
      <|> linkL RShift
      <|> linkL LShift
      <?> "bitwise shift operator"

-- $11.8 Relational Operators

relational_expression :: As3Parser Expression
relational_expression =
  chainl1 (tok shift_expression) relational_op <?> "relational expression"
  where
    relational_op :: As3Parser (Expression -> Expression -> Expression)
    relational_op = do
      true <- usein
      if true then relational_op_in else relational_op_noin 
    relational_op_in :: As3Parser (Expression -> Expression -> Expression)
    relational_op_in =
          linkL LessThanEq -- <= before <
      <|> linkL LessThan
      <|> linkL GreaterThanEq -- >= before >
      <|> linkL GreaterThan
      <|> linkL InstanceOf -- instanceof before in
      <|> linkL In
      <?> "relational operator"
    relational_op_noin :: As3Parser (Expression -> Expression -> Expression)
    relational_op_noin =
          linkL LessThanEq -- <= before <
      <|> linkL LessThan
      <|> linkL GreaterThanEq -- >= before >
      <|> linkL GreaterThan
      <|> linkL InstanceOf -- instanceof before in
      <?> "relational operator NO IN"

-- $11.9 Equality Operators

equality_expression :: As3Parser Expression
equality_expression =
  chainl1 (tok relational_expression) equality_op <?> "equality expression"
  where
    equality_op :: As3Parser (Expression -> Expression -> Expression)
    equality_op =
          linkL StrictEquality -- === before ==
      <|> linkL Equality
      <|> linkL StrictInEquality -- !== before !=
      <|> linkL InEquality
      <?> "equality operator"

-- $11.10 Binary Bitwise Operators

bitwiseAND_expression :: As3Parser Expression
bitwiseAND_expression =
  chainl1 (tok equality_expression) (try op) -- try op so part of && isn't consumed
  <?> "bitwise AND expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL BitwiseAND

bitwiseXOR_expression :: As3Parser Expression
bitwiseXOR_expression =
  chainl1 (tok bitwiseAND_expression) op <?> "bitwise XOR expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL BitwiseXOR

bitwiseOR_expression :: As3Parser Expression
bitwiseOR_expression =
  chainl1 (tok bitwiseXOR_expression) (try op)  -- try op so part of || isn't consumed
  <?> "bitwise OR expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL BitwiseOR

-- $11.11 Binary Logical Operators

logicalAND_expression :: As3Parser Expression
logicalAND_expression =
  chainl1 (tok bitwiseOR_expression) op <?> "logical AND expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL LogicalAND

logicalOR_expression :: As3Parser Expression
logicalOR_expression =
   chainl1 (tok logicalAND_expression) op <?> "logical OR expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL LogicalOR

-- $11.12 Conditional Operator ( ? : )

conditional_expression :: As3Parser Expression
conditional_expression =
  try (liftM3
         TernOp
         (tok logicalOR_expression <* tok (string "?"))
         (tok assignment_expression <* tok (string ":")) -- true
         (tok assignment_expression)) -- false
  <|> logicalOR_expression
  <?> "conditional expression"

-- $11.13 Assignment Operators

assignment_expression :: As3Parser Expression
assignment_expression =
  try (liftM3 RBinOp
         (tok lhs_expression)
         (tok assignment_op)
         (with_scope PS_UntypedIds assignment_expression))
  <|> conditional_expression
  <?> "assignment expression"
  where
    assignment_op :: As3Parser BinaryOp
    assignment_op =
          symR Assignment
      <|> symR PlusAssignment
      <|> symR MinusAssignment
      <|> symR MultiplicationAssignment
      <|> symR DivisionAssignment
      <|> symR ModuloAssignment
      <|> symR LShiftAssignment
      <|> symR RShiftAssignment
      <|> symR URShiftAssignment
      <|> symR BitwiseANDAssignment
      <|> symR BitwiseORAssignment
      <|> symR BitwiseXORAssignment
      <?> "assignment operator"

-- $11.14 Comma Operator

-- sepBy1. compare to other uses of Comma
comma_expression :: As3Parser Expression
comma_expression = liftM Comma (assignment_expression `sepBy1` comma)

expression :: As3Parser Expression
expression = comma_expression

expression_no_in :: As3Parser Expression
expression_no_in = noin expression

-- $Chain links

{-linkR :: BinaryOp -> As3Parser (Expression -> Expression -> Expression)
linkR = linkCommon RBinOp-}

linkL :: BinaryOp -> As3Parser (Expression -> Expression -> Expression)
linkL = linkCommon LBinOp

linkCommon :: (BinaryOp -> Expression -> Expression -> Expression)
           -> BinaryOp
           -> As3Parser (Expression -> Expression -> Expression)
linkCommon f op = try $ tok lop >> notFollowedBy lop >> return (f op)
  where
    lop = sym op

-- $Helpers

call :: (Expression -> As3Parser Expression) -> Expression -> As3Parser Expression
call loop l = do
  r <- char '.' *> var_id
  loop $ Call l r

array_access :: (Expression -> As3Parser Expression) -> Expression -> As3Parser Expression
array_access loop l = do
  r <- between_brackets expression
  loop $ ArrayAccess l r
