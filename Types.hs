module Types where

import Data.Map

-- Types
data Type = TInt | TBool | TStr | TLam Type Type deriving (Show, Eq)

-- Values
data Value = 
    VInt Int
  | VBool Bool
  | VStr String
    deriving (Show, Eq)

-- Expressions
data Expr = 
    EVal Value
  | EVar String           -- Variable
  | EDef String Expr      -- Binding
  | EIf Expr Expr Expr    -- if then else
  | ELet String Expr Expr -- let..in
  | ELam String Expr      -- \x -> e (lambda abstraction)
  | EAp Expr Expr         -- function application
  | EType String Type     -- type signature
    deriving (Show, Eq)

data FievelError = 
  Parser String
  deriving (Show)

data FievelState = FievelState{
  eBindings :: Map String Expr, -- expression bindings 
  tBindings :: Map String Type  -- type bindings
} deriving (Show, Eq)