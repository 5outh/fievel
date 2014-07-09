{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Map
import Control.Lens

-- Types
data Type = 
    TInt 
  | TBool 
  | TStr 
  | TLam Type Type 
    deriving (Show, Eq)

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
  | EOp PrimOp            -- primitive operator
  | EIf Expr Expr Expr    -- if then else
  | ELet String Expr Expr -- let..in (substitution)
  | ELam String Expr      -- \x -> e (lambda abstraction)
  | EAp Expr Expr         -- function application
  | EDef String Expr      -- Binding
  | EType String Type     -- type signature
    deriving (Show, Eq)

data PrimOp = 
    NNBO NumNumBinOp       -- Int -> Int -> Int 
  | SSBO StringStringBinOp -- String -> String -> String
  | NBBO NumBoolBinOp      -- Int -> Int -> Bool
  | BUO  BoolUnaryOp       -- Bool -> Bool
  | BBO  BoolBinOp         -- Bool -> Bool -> Bool 
    deriving (Show, Eq)

data NumNumBinOp =
    Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
    deriving (Show, Eq)

data StringStringBinOp = 
  Expr :<>: Expr
  deriving (Show, Eq)

data NumBoolBinOp =
    Expr :=: Expr
  | Expr :!=: Expr
  | Expr :<: Expr
  | Expr :>: Expr
  | Expr :>=: Expr
  | Expr :<=: Expr
    deriving (Show, Eq)

data BoolUnaryOp =
  BNot Expr
  deriving (Show, Eq)

data BoolBinOp = 
    Expr :|: Expr
  | Expr :&: Expr
    deriving (Show, Eq)

data FievelError = 
    Parser    String
  | TypeError String
    deriving (Show)

data FievelState = FievelState {
  _eBindings :: Map String Expr, -- expression bindings 
  _tBindings :: Map String Type  -- type bindings
} deriving (Show, Eq)

$(makeLenses ''FievelState)

emptyState :: FievelState
emptyState = FievelState (fromList []) (fromList [])