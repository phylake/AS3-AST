module Data.AS3.AST.Show () where

import           Control.Monad (liftM, liftM2)
import           Data.AS3.AST.Def hiding (M,p)
import           Data.Foldable (foldl)
import           Data.List (intersperse, intercalate)
import qualified MonadLib as ML

type M = ML.StateT Int ML.Id

class PrettyAs a where
  toAs3 :: a -> M String

instance Show Expression where
  show = fst . ML.runId . ML.runStateT 0 . toAs3

instance Show Statement where
  show = fst . ML.runId . ML.runStateT 0 . toAs3

inBlock :: (PrettyAs a) => [a] -> M String
inBlock action = do
  open <- p "{"
  body <- withIndent $ mapM toAs3 action
  close <- p "}"
  return $ unlines $ [open] ++ body ++ [close]

withIndent :: M a -> M a
withIndent action = do
  i <- ML.get
  ML.set (i+1)
  ret <- action
  ML.set i
  return ret

withNoIndent :: M a -> M a
withNoIndent action = do
  orig <- ML.get
  ML.set 0
  ret <- action
  ML.set orig
  return ret

indentChar :: Char
indentChar = '\t'

spaces :: M String
spaces = do
  i <- ML.get
  return $ replicate i indentChar

trimL :: String -> String
trimL = dropWhile (==indentChar)

p :: String -> M String
p = liftM2 (++) spaces . return

instance PrettyAs SwitchBody where
  toAs3 (Case e ms) = do
    e' <- toAs3 e
    ms' <- withIndent $ mapM toAs3 ms
    p $ "case " ++ e' ++ ":\n" ++ unlines ms'
  toAs3 (Default ms) = do
    ms' <- withIndent $ mapM toAs3 ms
    p $ "default:\n" ++ unlines ms'

instance Show Accessor where
  show Get = "get"
  show Set = "set"

instance PrettyAs Statement where
  toAs3 EmptyS = return ""
  toAs3 (Block sts) = inBlock sts
  toAs3 (Variable e) = do
    e' <- toAs3 e
    p $ "var " ++ e' ++ ";"
  toAs3 (Constant e) = do
    e' <- toAs3 e
    p $ "const " ++ e' ++ ";"
  toAs3 (ExpressionStmt e) = toAs3 e >>= p >>= return . (++";/*exp stmt*/")
  toAs3 (If e s@(Block _) elifs) = do
    e' <- withNoIndent $ toAs3 e
    s' <- toAs3 s
    elifs' <- mapM toAs3 elifs
    p $ "if (" ++ e' ++ ")\n" ++ s' ++ unlines elifs'
  toAs3 (If e s elifs) = do
    e' <- withNoIndent $ toAs3 e
    s' <- inBlock [s]
    elifs' <- mapM toAs3 elifs
    p $ "if (" ++ e' ++ ")\n" ++ s' ++ concat elifs'
  toAs3 (ElseIf e s@(Block _)) = do
    e' <- withNoIndent $ toAs3 e
    s' <- toAs3 s
    p $ "else if (" ++ e' ++ ")\n" ++ s'
  toAs3 (ElseIf e s) = do
    e' <- withNoIndent $ toAs3 e
    s' <- inBlock [s]
    p $ "else if (" ++ e' ++ ")\n" ++ s'
  toAs3 (Else s@(Block _)) = do
    s' <- toAs3 s
    p $ "else\n" ++ s'
  toAs3 (Else s) = do
    s' <- inBlock [s]
    p $ "else\n" ++ s'
  toAs3 (DoWhile s@(Block ss) e) = do
    e' <- toAs3 e
    open <- p "do {"
    body <- withIndent $ mapM toAs3 ss
    close <- p $ "} while (" ++ e' ++ ");"
    return $ unlines $ [open] ++ body ++ [close]
  toAs3 (DoWhile s e) = toAs3 $ DoWhile (Block [s]) e
  toAs3 (While e s) = do
    e' <- toAs3 e
    s' <- toAs3 s
    p $ "while (" ++ e' ++ ")\n" ++ s'
  toAs3 (For ms me1 me2 s) = do
    ms' <- maybe (return "") (withNoIndent . toAs3) ms
    me1' <- maybe (return "") toAs3 me1
    me2' <- maybe (return "") toAs3 me2
    s' <- toAs3 s
    case s of
      Block _ -> do
        for <- p $ "for (" ++ ms' ++ ";" ++ me1' ++ "; " ++ me2' ++ ")"
        return $ unlines [for, s']
      otherwise ->
        p $ "for (" ++ ms' ++ ";" ++ me1' ++ "; " ++ me2' ++ ") " ++ s' ++ ";"
  toAs3 (ForIn s e body) = do
    s' <- withNoIndent $ case s of
      ExpressionStmt es -> toAs3 es
      Variable ve -> toAs3 ve >>= p . ("var "++)
      otherwise -> toAs3 s
    e' <- toAs3 e
    body' <- case body of
      Block _ -> toAs3 body
      otherwise -> inBlock [body]
    p $ "for (" ++ s' ++ " in " ++ e' ++ ")\n" ++ body'
  toAs3 (ForEach s e body) = do
    s' <- withNoIndent $ case s of
      ExpressionStmt es -> toAs3 es
      Variable ve -> toAs3 ve >>= p . ("var "++)
      otherwise -> toAs3 s
    e' <- toAs3 e
    body' <- case body of
      Block _ -> toAs3 body
      otherwise -> inBlock [body]
    p $ "for each (" ++ s' ++ " in " ++ e' ++ ")\n" ++ body'
  toAs3 (Continue Nothing) = p "continue;"
  toAs3 (Continue (Just e)) = do
    e' <- toAs3 e
    p $ "continue " ++ e' ++ ";"
  toAs3 (Break Nothing) = p "break;"
  toAs3 (Break (Just e)) = do
    e' <- toAs3 e
    p $ "break " ++ e' ++ ";"
  toAs3 (Return Nothing) = p "return;"
  toAs3 (Return (Just e)) = do
    e' <- toAs3 e
    p $ "return " ++ e' ++ ";"
  toAs3 (With e s@(Block _)) = do
    e' <- withNoIndent $ toAs3 e
    s' <- toAs3 s
    p $ "with (" ++ e' ++ ")\n" ++ s'
  toAs3 (With e s) = do
    e' <- withNoIndent $ toAs3 e
    s' <- inBlock [s]
    p $ "with (" ++ e' ++ ")\n" ++ s'
  toAs3 (Switch e bs) = do
    e' <- toAs3 e
    bs' <- inBlock bs
    p $ "switch (" ++ e' ++ ")\n" ++ bs'
  toAs3 (Labeled ident s) = do
    s' <- toAs3 s
    p $ ident ++ ": " ++ trimL s'
  toAs3 (Package a body) = do
    body <- inBlock body
    return $ unlines ["package" ++ maybe "" ((++)" ") a, body]
  toAs3 (Import a) = p $ "import " ++ a ++ ";"
  toAs3 (Class scopes name extends implements body) = do
    dec <- p $ intercalate " " (map show scopes) ++ " class " ++ name
               ++ maybe "" (" extends "++) extends
               ++ maybe "" (\i -> " implements " ++ intercalate ", " i) implements
               ++ "\n"
    body' <- inBlock body
    return $ dec ++ body'
  toAs3 (FnDec scopes mAccessor name params t body) = do
    body <- withIndent $ mapM toAs3 body
    params <- withNoIndent $ mapM toAs3 params
    p $
      intercalate " " (map show scopes) ++ " function " ++ name
      ++ maybe "" show mAccessor
      ++ "(" ++ intercalate ", " params ++ "):" ++ show t ++ "\n"
      ++ "\t\t{\n" ++ unlines body ++ "\n\t\t}"

instance PrettyAs Expression where
  toAs3 (TODO_E a) = return $ "TODO[" ++ a ++ "]"
  toAs3 This = return "this"
  toAs3 (Comma a) = liftM (intercalate ", ") $ mapM toAs3 a
  toAs3 (ParenGroup a) = do
    a' <- toAs3 a
    return $ "(" ++ a' ++ ")"
  toAs3 (ObjectLiteral e) = do
    e' <- toAs3 e
    return $ "{" ++ e' ++ "}"
  toAs3 (ArrayLiteral e) = do
    e' <- toAs3 e
    return $ "[" ++ e' ++ "]"
  toAs3 (KeyValue k v) = do
    k <- toAs3 k
    v <- toAs3 v
    return $ k ++ ":" ++ v
  toAs3 (TernOp cond t f) = do
    cond <- toAs3 cond
    t <- toAs3 t
    f <- toAs3 f
    return $ cond ++ " ? " ++ t ++ " : " ++ f
  toAs3 (RBinOp l op r) = do
    l' <- toAs3 l
    r' <- toAs3 r
    return $ intercalate " " [l', show op, r']
  toAs3 (LBinOp op l r) = do
    l' <- toAs3 l
    r' <- toAs3 r
    return $ intercalate " " [l', show op, r']
  toAs3 (Unary op e) = liftM2 (++) (return $ show op) (toAs3 e)
  toAs3 (ClassId ms cv n t) = return $
    intercalate " " (map show ms ++ [show cv, n]) ++ ":" ++ show t
  toAs3 (FnId cv n t) = return $ intercalate " " [show cv, n] ++ ":" ++ show t
  toAs3 (TypedId n t) = return $ n ++ ":" ++ show t
  toAs3 (UntypedId a) = return a
  toAs3 (Lit a) = return $ show a
  toAs3 (Postfix e op) = liftM2 (++) (toAs3 e) (return $ show op)
  toAs3 (ArrayAccess a b) = do
    a' <- toAs3 a
    b' <- toAs3 b
    return $ a' ++ "[" ++ b' ++ "]"
  toAs3 (Call a b) = do
    a' <- toAs3 a
    return $ a' ++ "." ++ b
  toAs3 (CallEMember a b) = do
    a' <- toAs3 a
    b' <- toAs3 b
    return $ a' ++ "(" ++ b' ++ ")"
  toAs3 (CallEArguments a b) = do
    a' <- toAs3 a
    b' <- toAs3 b
    return $ a' ++ "(" ++ b' ++ ")"

instance Show BinaryOp where
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"
  show Modulo = "%"

  show LShift = "<<"
  show RShift = ">>"
  show URShift = ">>>"

  show LogicalAND = "&&"
  show LogicalOR = "||"

  show BitwiseAND = "&"
  show BitwiseOR = "|"
  show BitwiseXOR = "^"

  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanEq = "<="
  show GreaterThanEq = ">="
  show InstanceOf = "instanceof"
  show In = "in"

  show Assignment = "="
  show PlusAssignment = "+="
  show MinusAssignment = "-="
  show MultiplicationAssignment = "*="
  show DivisionAssignment = "/="
  show ModuloAssignment = "%="
  show LShiftAssignment = "<<="
  show RShiftAssignment = ">>="
  show URShiftAssignment = ">>>="
  show BitwiseANDAssignment = "&="
  show BitwiseORAssignment = "|="
  show BitwiseXORAssignment = "^="

  show Equality = "=="
  show StrictEquality = "==="
  show InEquality = "!="
  show StrictInEquality = "!=="

instance Show UnaryOp where
  show Delete = "delete"
  show Void = "void"
  show TypeOf = "typeof"
  show Increment = "++"
  show Decrement = "--"
  show Positive = "+"
  show Negative = "-"
  show BitwiseNOT = "~"
  show LogicalNOT = "!"

instance Show Literal where
  show L_void = "void"
  show L_undefined = "undefined"
  show (L_int a) = show a
  show (L_uint a) = show a
  show (L_Number a) = show a
  show (L_Boolean True) = "true"
  show (L_Boolean False) = "false"
  show (L_String a) = a
  show (L_RegExp a) = a

instance Show Type where
  show T_int = "int"
  show T_uint = "uint"
  show T_void = "void"
  show T_undefined = "*"
  show T_Number = "Number"
  show T_Boolean = "Boolean"
  show T_String = "String"
  show T_Array = "Array"
  show T_Object = "Object"
  show (T_Vector t) = "Vector.<" ++ show t ++ ">"
  show (T_UserDefined t) = t

instance Show ScopeMod where
  show Public = "public"
  show Protected = "protected"
  show Private = "private"
  show Final = "final"
  show Override = "override"
  show Static = "static"

instance Show CV where
  show Const = "const"
  show Var = "var"
