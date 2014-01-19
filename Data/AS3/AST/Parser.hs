module Data.AS3.AST.Parser (parser) where

import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Statements

parser :: As3Parser Statement
parser = package

type_defaults :: Maybe String -> Type -> Maybe String
type_defaults init T_int =             Just $ maybe "0"     id init
type_defaults init T_Number =          Just $ maybe "0.0"   id init
type_defaults init T_Boolean =         Just $ maybe "false" id init
type_defaults init T_String =          Just $ maybe ""      id init
type_defaults init (T_Vector _) =      Just $ maybe "null"  id init
type_defaults init (T_UserDefined _) = Just $ maybe "null"  id init
