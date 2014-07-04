{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as T
import Control.Applicative hiding (optional, many)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as M
import Types

fievelDef = LanguageDef {
  commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = False
  , identStart     = letter <|> oneOf "_'"
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = oneOf "~!@#$%^&*|:<>?=+-\\"
  , opLetter       = oneOf "~!@#$%^&*|:<>?=+-"
  , reservedNames  = [ "if", "then", "else", "let", "be", "in", "Int", "Bool", "String"
                     , "True", "False"]
  , reservedOpNames = [ "+", "-", "*", "^", "/", "<>", "!", ":=", "=", "!="
                      , ">", "<", ">=", "<=", "'", "\"", "->" ]
  , caseSensitive = True
}

-- Lexer
lexer = T.makeTokenParser fievelDef
reserved = T.reserved lexer

-- Expression parsers
bool = (reserved "True"  *> pure (EVal . VBool $ True))
    <|> (reserved "False" *> pure (EVal . VBool $ False))

int = liftA (EVal . VInt . fromIntegral) $ T.integer lexer

str = liftA (EVal . VStr) $ T.stringLiteral lexer

ifte = do
  reserved "if"
  t1 <- fievelExpr
  reserved "then"
  t2 <- fievelExpr
  reserved "else"
  t3 <- fievelExpr
  return $ EIf t1 t2 t3

letExpr = do
  reserved "let"
  name <- T.identifier lexer
  reserved "be"
  t1 <- fievelExpr
  reserved "in"
  t2 <- fievelExpr
  return $ ELet name t1 t2

-- @TODO: Expressions
defn = do
  name <- T.identifier lexer
  T.reservedOp lexer ":="
  expr <- fievelExpr
  return $ EVar name expr

fievelExpr =
    choice 
  $ ((<|>) <$> id <*> T.parens lexer) 
  <$> [ifte, bool, int, str, defn, letExpr]

-- Type parsers
constType =
        reserved "Int"    *> pure TInt
    <|> reserved "Bool"   *> pure TBool
    <|> reserved "String" *> pure TStr

fievelType = 
  liftA (foldr1 TLam) 
  ( (constType <|> T.parens lexer fievelType) 
    `sepBy` 
    (T.reservedOp lexer "->") )

typeSig = do
  name <- T.identifier lexer
  T.colon lexer
  typ <- fievelType
  return $ EType name typ

parseFievel = parse (typeSig <|> fievelExpr) "(fievel)"