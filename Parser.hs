{-# LANGUAGE NoMonomorphismRestriction #-}

{-
So here's the thing: I think LanguageDefs aren't used for manipulating
source files directly, they're only good for parsing out the actual expressions.

A source file consists of *several* "programs" -- function definitions that I can parse using
a LanguageDef, and same goes for the type declarations. However, I don't want to lex everything together...that's bad.

NOTE: We can parse individual lines + the lines that are preceded by spaces or tabs as single expressions
(type level or normal), and parse into these *first*
-}
module Parser where

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as T
import Control.Applicative hiding (optional, many, (<|>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.Maybe
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
                     , "True", "False", "run" ]
  , reservedOpNames = [ "+", "-", "*", "^", "/", "<>", "!", ":=", "=", "!="
                      , ">", "<", ">=", "<=", "'", "\"", "->", "." ]
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

lambda = do
  T.reservedOp lexer "\\"
  name <- T.identifier lexer
  T.reservedOp lexer "->"
  exprs <- many1 fievelExpr
  return $ ELam Nothing name (foldr1 EAp exprs)

defn = do
  name <- T.identifier lexer <|> (reserved "run" >> return "run")
  T.reservedOp lexer ":="
  expr <- fievelExpr
  return $ EDef name expr

fundefn = do
  (f:args) <- many1 (T.identifier lexer)
  T.reservedOp lexer ":="
  exprs <- many1 fievelExpr
  return $ EDef f (foldr (ELam Nothing) (foldr1 EAp exprs) args)

variable = liftA (EVar Nothing) $ T.identifier lexer

fievelExpr =
  let paren = T.parens lexer 
      trys  = ((<|>) <$> try <*> (try . paren)) <$> [fundefn, defn, ifte, lambda]
      base  = ((<|>) <$> id <*> paren) <$> [letExpr, variable, bool, int, str]
  in choice (trys ++ base)

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

-- (Maybe Type, Expr)
exprDef = do
  typ  <- optionMaybe $ try typeSig
  expr <- fievelExpr
  T.reservedOp lexer "."
  return (typ, expr)

-- parse manually
parseFievel :: String -> Either FievelError (Maybe Expr, Expr) 
parseFievel src = case parse exprDef "(fievel)" src of
  Left err -> Left $ Parser (show err)
  Right e  -> Right e

typeToTuple :: Expr -> Maybe (String, Type)
typeToTuple (EType nm tp) = Just $ (nm, tp)
typeToTuple _             = Nothing

defToTuple :: Expr -> Maybe (String, Expr)
defToTuple  (EDef a val)  = Just $ (a, val)
defToTuple  _             = Nothing

-- Populate the De Bruijn indices of a lambda
populateDebruijn :: Expr -> Expr
populateDebruijn = shift M.empty
  where shift ctx (EVar n v) = EVar (Just (ctx M.! n')) v
          where n' = maybe 0 id n
        shift ctx (EAp a b)  = EAp (shift ctx a) (shift ctx b)
        shift ctx (ELam n nm body) = ELam (Just n') nm (shift c' body)
          where n' = maybe 0 id n
                c' = M.insert n' 0 (M.map (+1) ctx)

-- parse from file
parseFievelFile :: FilePath -> IO (Either FievelError FievelState)
parseFievelFile file = do
  src <- parseFromFile (many exprDef) file
  case src of
    Left err -> return . Left . Parser $ show err
    Right xs -> let (ts, es) = (catMaybes $ map fst xs, map snd xs)
                in return . Right $ FievelState
                    (M.fromList $ catMaybes $ map defToTuple es)
                    (M.fromList $ catMaybes $ map typeToTuple ts)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t'

chunk acc [] = [acc]
chunk acc (x:xs) 
  | null x = chunk acc xs
  | isWhitespace (head x) = chunk (acc ++ (' ': dropWhile isWhitespace x)) xs
  | otherwise = acc : chunk x xs 

chunkify :: FilePath -> IO [String]
chunkify file = do
  contents <- lines <$> readFile file
  return $ filter (not . null) $ chunk [] contents

typeBinding = do
  name <- T.identifier lexer
  T.colon lexer
  typ <- fievelType
  return $ (name, typ)

expressionBinding = do
  def <- choice $ (<|>) <$> try <*> (try . paren) <$> [fundefn, defn]
  case defToTuple def of
    Just tpl -> return tpl
    _        -> error "Parsing failed; internal error."
  where paren = T.parens lexer

data ParseResult = 
    PType (String, Type)
  | PExpr (String, Expr)
    deriving (Show, Eq)

partition :: [ParseResult] -> ([(String, Type)], [(String, Expr)])
partition []              = ([], [])
partition (PType st : xs) = let (ts, es) = partition xs in (st:ts, es)
partition (PExpr se : xs) = let (ts, es) = partition xs in (ts, se:es)

parseFievelExpr = 
  try (PExpr  <$> expressionBinding)
  <|>  PType  <$> typeBinding

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _ ) = error "Attempt to extract Right value from Left"

parseFievelFromFile :: FilePath -> IO FievelState
parseFievelFromFile file = do
  exprs <- map (parse parseFievelExpr "(from file)") <$> chunkify file
  guard (all isRight exprs)
  let (ts, es) = partition $ fromRight <$> exprs
  return $ FievelState (M.fromList es) (M.fromList ts)

