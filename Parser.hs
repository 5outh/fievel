{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative hiding (optional, many)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Map as M
import Types

addExprBinding :: String -> Expr -> FievelState -> FievelState
addExprBinding str e (FievelState es vs) = FievelState (M.insert str e es) vs

addTypeBinding :: String -> Type -> FievelState -> FievelState
addTypeBinding str e (FievelState es vs) = FievelState es (M.insert str e vs)

alpha = many $ oneOf ['A'..'z']

fievelType = do
  typ <- alpha
  return $ case typ of
    "Int"    -> TInt
    "Bool"   -> TBool
    "String" -> TStr
    x        -> error $ "Not yet implemented: Lambda, or invalid type: " ++ x

typeSig = do
  name <- alpha
  many1 space
  char ':'
  many1 space
  typ <- fievelType
  (optional spaces)
  return $ EType name typ

variable = do
  name <- alpha
  many1 space
  string ":="
  many1 space
  expr <- fievelExpr
  optional spaces
  return $ EVar name expr

fievelExpr = 
      bool
  <|> int
  <|> str
  <|> ifte
  <|> letExpr

bool = liftA (EVal . VBool . read) $ string "True" <|> string "False"
int  = liftA (EVal . VInt . read) $ many1 digit
str  = liftA (EVal . VStr . read) $ char '\"' *> many1 anyChar <* char '\"'

ifte = do
  string "if"
  many1 space
  e1 <- fievelExpr
  many1 space
  string "then"
  many1 space
  e2 <- fievelExpr
  many1 space
  string "else"
  many1 space
  e3 <- fievelExpr
  return $ EIf e1 e2 e3

letExpr = do
  string "let"
  many1 space
  name <- alpha
  many1 space >> char '='
  many1 space
  e1 <- fievelExpr
  many1 space
  string "in"
  many1 space
  e2 <- fievelExpr
  optional spaces
  return $ ELet name e1 e2

test = parse (variable <|> typeSig <|> fievelExpr) "(fievel)"