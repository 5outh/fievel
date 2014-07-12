{-# LANGUAGE MultiWayIf, TupleSections #-}
{-

Idea: Use a Zipper for type inference on variables; introduce zipper types for each
      spot in the AST that a value could have come from. When we hit a Var, go up one step
      and check the appropriate other spots. For example, say we have the expression:

      \y -> \x -> y + x
      
      == ELam "y" (ELam "x" (EVar "y" :+: EVar "x"))

      We see a lambda with an expression and descend. We see another lambda with another expression and descend again.
      Now we see (e :+: e'). We first descend into e, to find EVar "y". We note that we got there from the first hole
      in :+:, so y must be a TInt. Add this to the context. We do the same for e' with x; it must be a TInt.

      We now unfold and tell the inner Lambda that x : TInt. Now this expression has type TLam TInt TInt (Int -> Int).
      We now note that the original inner expression has type TLam TInt TInt, and find that y : TInt. Now the outer
      expression has type TLam TInt (TLam TInt TInt) (Int -> Int -> Int) and we are done.

      A Zipper type may encapsulate every single location that is possible within the AST. It may, however, be possible
      to just note that we're coming from a position that implies that any variable in that position must have a 
      specific type (i.e. a Var coming from the first position in a :+: constructor has type TInt no matter what).
      But, it's not always that simple! Consider, for example, if a then b else c, where we want the type of b. This 
      necessarily depends on the type of c -- it's not a constant. How would we handle that (I don't know)?

      In general, though, the idea would be to not automatically assume a variable is unbound if it is encountered.
      We instead want to get the *inferred* type of the variable, when appropriate*

      I don't know when it is appropriate.
-}
module TypeChecker where

import Data.Map as M
import Control.Monad.State

import Types

-- Pairs of bound variables to their inferred types
type Context     = M.Map String Type
type Contextual  = State Context
-- Either infers the type of a variable or throws a contradiction error.
type TypeChecker = Contextual (Either FievelError Type)

-- EVal Value 
-- EVar String           -- Variable                     -- ** updates bindings in scope
-- EOp PrimOp            -- primitive operator 
-- EIf Expr Expr Expr    -- if then else 
-- ELet String Expr Expr -- let..in (substitution)       -- ** the dreaded
-- ELam String Expr      -- \x -> e (lambda abstraction) -- ** connected to scope
-- EAp Expr Expr         -- function application
-- EDef String Expr      -- Binding        ** not needed
-- EType String Type     -- type signature ** not needed

data ASTCrumb = 
    If1 Expr Expr -- => Bool
  | If2 Expr Expr-- => typeOf if3
  | If3 Expr Expr-- => typeOf if2
  | If2Var Expr Expr -- => typeof expr
  | If3Var Expr Expr -- => typeof expr
  | Let1 String Expr -- => no inference, don't bind
  | Let2 String Expr -- => no inference, don't bind
  | Lam String  -- => typeOf var in context, else undecidable
  | Ap1 Expr  -- => Definitely a lambda, otherwise depends on Ap1
  | Ap2 Expr  -- => Depends on type of Ap1
  -- Int -> Int -> Int
  | Plus1 Expr -- => Int
  | Plus2 Expr -- => Int
  | Minus1 Expr -- => Int
  | Minus2 Expr -- => Int
  | Times1 Expr -- => Int
  | Times2 Expr -- => Int
  | Div1 Expr -- => Int
  | Div2 Expr -- => Int
  -- String -> String -> String
  | Concat1 Expr -- => String
  | Concat2 Expr -- => String
  -- Int -> Int -> Bool
  | Equal1 Expr -- => Int
  | Equal2 Expr -- => Int
  | NEqual1 Expr -- => Int
  | NEqual2 Expr -- => Int
  | LT1 Expr -- => Int
  | LT2 Expr -- => Int
  | GT1 Expr -- => Int
  | GT2 Expr -- => Int
  | GEQ1 Expr -- => Int
  | GEQ2 Expr -- => Int
  | LTE1 Expr -- => Int
  | LTE2 Expr -- => Int
  -- Bool -> Bool -> Bool
  | Or1 Expr -- => Bool
  | Or2 Expr -- => Bool
  | And1 Expr -- => Bool
  | And2 Expr -- => Bool
  -- Bool -> Bool
  | Not -- => Bool
    deriving (Show, Eq)

-- How to traverse back
type Crumbs    = [ASTCrumb]

-- Current expression, last crumb
data ASTZipper = ASTZipper Expr Crumbs

goUp :: ASTZipper -> ASTZipper
goUp z@(ASTZipper _ []) = z
goUp (ASTZipper e (c:cs)) = (flip ASTZipper cs) $ case c of
    If1 e1 e2  -> EIf e e1 e2
    If2 e1 e2  -> EIf e1 e e2
    If3 e1 e2  -> EIf e1 e2 e
    If2Var e1 e2 -> EIf e1 e e2
    If3Var e1 e2 -> EIf e1 e2 e
    -- ^ Variables are undecided.
    Let1 s e1  -> ELet s e e1
    Let2 s e1  -> ELet s e1 e
    Lam str    -> ELam str e
    Ap1 e1     -> EAp e e1
    Ap2 e1     -> EAp e1 e
    Plus1 e1   -> EOp $ e  :+: e1
    Plus2 e1   -> EOp $ e1 :+: e
    Minus1 e1  -> EOp $ e  :-: e1
    Minus2 e1  -> EOp $ e1 :-:  e
    Times1 e1  -> EOp $ e  :*: e1
    Times2 e1  -> EOp $ e1 :*:  e
    Div1 e1    -> EOp $ e  :/: e1
    Div2 e1    -> EOp $ e1 :/:  e
    Concat1 e1 -> EOp $ e  :<>: e1
    Concat2 e1 -> EOp $ e1 :<>:  e
    Equal1 e1  -> EOp $ e  :=: e1
    Equal2 e1  -> EOp $ e1 :=:  e
    NEqual1 e1 -> EOp $ e  :!=: e1
    NEqual2 e1 -> EOp $ e1 :!=:  e
    LT1 e1     -> EOp $ e  :<: e1
    LT2 e1     -> EOp $ e1 :<:  e
    GT1 e1     -> EOp $ e  :>: e1
    GT2 e1     -> EOp $ e1 :>:  e
    GEQ1 e1    -> EOp $ e  :>=: e1
    GEQ2 e1    -> EOp $ e1 :>=:  e
    LTE1 e1    -> EOp $ e  :<=: e1
    LTE2 e1    -> EOp $ e1 :<=:  e
    Or1 e1     -> EOp $ e  :|: e1
    Or2 e1     -> EOp $ e1 :|:  e
    And1 e1    -> EOp $ e  :&: e1
    And2 e1    -> EOp $ e1 :&:  e
    Not        -> EOp $ BNot e

-- Given the position a variable came from, infer its type.
getVarType :: ASTCrumb -> Type
getVarType c = case c of
  If1 _ _   -> TBool
  Plus1 e1   -> TInt
  Plus2 e1   -> TInt
  Minus1 e1  -> TInt
  Minus2 e1  -> TInt
  Times1 e1  -> TInt
  Times2 e1  -> TInt
  Div1 e1    -> TInt
  Div2 e1    -> TInt
  Concat1 e1 -> TStr
  Concat2 e1 -> TStr
  Equal1 e1  -> TInt
  Equal2 e1  -> TInt
  NEqual1 e1 -> TInt
  NEqual2 e1 -> TInt
  LT1 e1     -> TInt
  LT2 e1     -> TInt
  GT1 e1     -> TInt
  GT2 e1     -> TInt
  GEQ1 e1    -> TInt
  GEQ2 e1    -> TInt
  LTE1 e1    -> TInt
  LTE2 e1    -> TInt
  Or1 e1     -> TBool
  Or2 e1     -> TBool
  And1 e1    -> TBool
  And2 e1    -> TBool
  Not        -> TBool
  If2 _ e2  -> undefined -- type of e2
  If3 _ e2  -> undefined -- type of e2
  If2Var _ _ -> undefined -- type of e, otherwise undefined
  If3Var _ _ -> undefined -- type of e, otherwise undefined
  Let1 s e1 -> undefined
  Let2 s e1 -> undefined
  Lam str   -> undefined -- actually errory
  Ap1 e1    -> undefined
  Ap2 e2    -> undefined

isUndecided :: Expr -> Bool
isUndecided (EVar _) = True
isUndecided _       = False

--infer :: ASTZipper -> Context -> Context
--infer (EVar a, (If1 _ _):_) ctx = M.insert a TBool ctx
--infer _ _ = undefined
---- and so on

typeOf :: Expr -> Either FievelError Type
typeOf (EVal (VInt _))  = Right TInt
typeOf (EVal (VBool _)) = Right TBool
typeOf (EVal (VStr _))  = Right TStr
typeOf (EOp op)         = typeOfOp op
typeOf (EIf e1 e2 e3)   = typeOfIf e1 e2 e3
typeOf (ELam v e)       =
  let (maybeErr, bindings) = runState (getBindings e) M.empty
  in case maybeErr of 
    Just err -> Left err
    Nothing -> case M.lookup v bindings of
      Nothing -> Left . TypeError $ "Variable not used in scope: " ++ v
      Just t -> case (typeOf e) of
        err@(Left _) -> err
        Right t'     -> Right $ TLam t t'
typeOf (EVar s)         = Left . TypeError $ "Variable not in scope: " ++ s
typeOf _                = Left . InternalError $ "Not yet implemented."

typeOfIf :: Expr -> Expr -> Expr -> Either FievelError Type
typeOfIf e1 e2 e3 = 
  let t1 = typeOf e1
      t2 = typeOf e2
      t3 = typeOf e3
  in case t1 of
    l@(Left _) -> l
    (Right t) -> 
      if | t /= TBool -> Left . TypeError $ "Expected TBool but got " ++ show t ++ " in an if-statement, namely " ++ show (EIf e1 e2 e3)
         | otherwise -> case (t2, t3) of
          (Right t2', Right t3') -> 
            if t2' == t3'
            then Right t2'
            else Left . TypeError $ "Mismatched types in if-statement: (" ++ show t2' ++ " != " ++ show t3' ++ "), namely " ++ show (EIf e1 e2 e3) 
          (l@(Left t), _) -> l
          (_, l@(Left t)) -> l

-- Operation type checking
typeOfOp :: PrimOp -> Either FievelError Type
typeOfOp op = 
  let chkNNBO = checkBinOp TInt TInt TInt
      chkNBBO = checkBinOp TInt TInt TBool
      chkSSBO = checkBinOp TStr TStr TStr
      chkBBO  = checkBinOp TBool TBool TBool
      chkBUO  = checkUnaryOp TBool TBool
  in case op of
    e1 :+: e2  -> chkNNBO e1 e2 "+"
    e1 :-: e2  -> chkNNBO e1 e2 "-"
    e1 :*: e2  -> chkNNBO e1 e2 "*"
    e1 :/: e2  -> chkNNBO e1 e2 "/"
    e1 :<>: e2 -> chkSSBO e1 e2 "<>"
    e1 :=: e2  -> chkNBBO e1 e2 "="
    e1 :!=: e2 -> chkNBBO e1 e2 "!="
    e1 :<: e2  -> chkNBBO e1 e2 "<"
    e1 :>: e2  -> chkNBBO e1 e2 ">"
    e1 :>=: e2 -> chkNBBO e1 e2 ">="
    e1 :<=: e2 -> chkNBBO e1 e2 "<="
    e1 :|: e2  -> chkBBO e1 e2 "|"
    e1 :&: e2  -> chkBBO e1 e2 "&"
    BNot e     -> chkBUO e "!"

-- Check a binary operator for well-typedness; throw type error if ill-typed.
checkBinOp :: Type -> Type -> Type -> Expr -> Expr -> String -> Either FievelError Type
checkBinOp et1 et2 ft e1 e2 opname = 
  let t1 = typeOf e1
      t2 = typeOf e2
  in case (t1, t2) of
    (Right t, Right t') -> 
      if | t == et1 && t' == et2 -> Right ft
         | t  /= et1 -> Left . TypeError $ "Expected " ++ show et1 ++ " but got " ++ show t ++ " in the first argument of " ++ opname ++ ", namely " ++ show e1
         | t' /= et2 -> Left . TypeError $ "Expected " ++ show et2 ++ " but got " ++ show t' ++ " in the second argument of " ++ opname ++ ", namely " ++ show e2
    (l@(Left t), _) -> l
    (_, l@(Left t)) -> l

checkUnaryOp :: Type -> Type -> Expr -> String -> Either FievelError Type
checkUnaryOp et ft e opname = 
  let t = typeOf e
  in case t of
    Right t'   -> if t' == et 
                  then Right ft 
                  else Left . TypeError $ "Expected " ++ show et ++ " but got " ++ show t' ++ " in the only argument of " ++ opname ++ ", namely " ++ show e 
    l@(Left _) -> l

-- Note: We need an inference mechanism.
-- some simple cases to consider:
-- \x -> x + x
-- x + x => x : Int, and x + x : Int, so \x -> x + x : Int -> Int
-- 
-- \a -> !a
-- !a => a : Bool, and !a : Bool, so \a -> !a : Bool -> Bool
-- 
-- \a -> \b -> a + b
-- \a -> \b -> if a then b else 10
-- A bit trickier.
-- if a .. => a : Bool
-- .. else 10 => b : typeOf 10 = Int
-- => a : Bool, b : Int, and if a then b else 10 : Int,
-- so \a -> \b -> if a then b else 10 : Bool -> Int -> Int
--
-- So what does inference look like?
-- A typing judgement x : Int can be an insertion into binding Context, i.e.
-- x : Int => M.insert "x" TInt ctx
-- Can keep track of the current context as we go through use of State.
-- The idea would be to collect a context from the inner expression,
-- then look up the variable in question at the end, while getting the
-- type of the inner expression in the process. Then the type would be
-- t -> (typeOf inner expression) for t = typeOf (var x in expr) 
--
-- A typing judgement can also just be a return value, i.e.
-- (This may need to traverse the AST multiple times)
-- getTypeOf "x" (\x -> x + x) = TInt 
-- 
-- 
-- Idea:
-- On Lambda, run type-inferencer on sub-expression and build context (in State)
-- Once it's done, a full context will be there! Look up the variable in question
-- and if it doesn't exist in the subexpression it's undefined what the type of 
-- the variable is (i.e. forall a. a)
--

-- @TODO: Cover all cases.
-- NB. Can run getBindings with an initial state if needed; i.e. for let..in expressions.
-- NB. Nothing in this case signals a *success*, contrary to what it may seem.
getBindings :: Expr -> State Context (Maybe FievelError)
getBindings expr = case expr of
  EOp op          -> getOpBindings op
  ELam _ e        -> getBindings    e
  e@(EIf b e1 e2) -> getIfBindings  e
  e@(ELet _ _ _ ) -> error "Not yet implemented: getBindings for Let expressions"-- this one's hard
  e@(EAp  _ _   ) -> error "Not yet implemented: getBindings for application" -- this one too
  EVal v          -> return Nothing
  EVar str        -> return . Just . TypeError $ "Variable " ++ show str ++ " is not bound."
  EDef def e      -> return . Just . TypeError $ "Encountered top-level definition for " ++ show def
  EType v t       -> return . Just . TypeError $ "Encountered type signature for " ++ show v

getIfBindings :: Expr -> State Context (Maybe FievelError)
getIfBindings e@(EIf _ (EVar b) (EVar c)) = 
  return . Just . TypeError $ "Could not deduce singular type for branches of if-statement, namely " ++ show e

getIfBindings (EIf (EVar a) (EVar b) e2) = do
  modify (M.insert a TBool)
  maybeErr <- getBindings e2
  case maybeErr of
    err@(Just _) -> return err
    Nothing -> case (typeOf e2) of
      Right t -> modify (M.insert b t) >> return Nothing
      Left err -> return . Just $ err

getIfBindings (EIf (EVar a) e1 (EVar c)) = do
  modify (M.insert a TBool)
  maybeErr <- getBindings e1
  case maybeErr of
    err@(Just _) -> return err
    Nothing -> case (typeOf e1) of
      Right t -> modify (M.insert c t) >> return Nothing
      Left err -> return . Just $ err

getIfBindings (EIf (EVar a) e1 e2) = do
  modify (M.insert a TBool)
  maybeErr <- getBindings e1
  case maybeErr of
    Nothing      -> getBindings e2
    err@(Just _) -> return err

getIfBindings (EIf a (EVar b) e2) = do
  maybeErr <- getBindings a
  case maybeErr of
    err@(Just _) -> return err
    Nothing -> do 
      maybeErr' <- getBindings e2
      case maybeErr' of
        err@(Just _) -> return err
        Nothing -> case (typeOf e2) of
          Right t -> modify (M.insert b t) >> return Nothing
          Left err -> return . Just $ err

getIfBindings (EIf a e1 (EVar c)) = do
  maybeErr <- getBindings a
  case maybeErr of
    err@(Just _) -> return err
    Nothing -> do 
      maybeErr' <- getBindings e1
      case maybeErr' of
        err@(Just _) -> return err
        Nothing -> case (typeOf e1) of
          Right t -> modify (M.insert c t) >> return Nothing
          Left err -> return . Just $ err

getIfBindings (EIf a b c) = do
  mErr <- getBindings a
  case mErr of
    err@(Just _) -> return err
    Nothing -> do
      mErr' <- getBindings b
      case mErr' of
        err@(Just _) -> return err
        Nothing -> getBindings c

ins :: Type -> String -> Expr -> State Context (Maybe FievelError)
ins t a e = modify (M.insert a t) >> getBindings e

-- NB. all cases covered here.
getOpBindings :: PrimOp -> State Context (Maybe FievelError)
getOpBindings op = 
  let int  = ins TInt
      bool = ins TBool
      str  = ins TStr
      gb e e' = do 
        maybeErr <- getBindings e
        case maybeErr of 
          Nothing      -> getBindings e'
          err@(Just _) -> return err
  in case op of
    (EVar a) :+: e  -> int a e
    (EVar a) :-: e  -> int a e
    (EVar a) :*: e  -> int a e
    (EVar a) :/: e  -> int a e
    (EVar a) :<>: e -> str a e
    (EVar a) :=: e  -> int a e
    (EVar a) :!=: e -> int a e
    (EVar a) :<: e  -> int a e
    (EVar a) :>: e  -> int a e
    (EVar a) :>=: e -> int a e
    (EVar a) :<=: e -> int a e
    (EVar a) :|: e  -> bool a e
    (EVar a) :&: e  -> bool a e
    e :+: (EVar a)  -> int a e
    e :-: (EVar a)  -> int a e
    e :*: (EVar a)  -> int a e
    e :/: (EVar a)  -> int a e
    e :<>: (EVar a) -> str a e
    e :=: (EVar a)  -> int a e
    e :!=: (EVar a) -> int a e
    e :<: (EVar a)  -> int a e
    e :>: (EVar a)  -> int a e
    e :>=: (EVar a) -> int a e
    e :<=: (EVar a) -> int a e
    e :|: (EVar a)  -> bool a e
    e :&: (EVar a)  -> bool a e
    e :+: e'        -> gb e e'
    e :-: e'        -> gb e e'
    e :*: e'        -> gb e e'
    e :/: e'        -> gb e e'
    e :<>: e'       -> gb e e'
    e :=: e'        -> gb e e'
    e :!=: e'       -> gb e e'
    e :<: e'        -> gb e e'
    e :>: e'        -> gb e e'
    e :>=: e'       -> gb e e'
    e :<=: e'       -> gb e e'
    e :|: e'        -> gb e e'
    e :&: e'        -> gb e e'
    BNot (EVar a)   -> modify (M.insert a TBool) >> return Nothing
    BNot e          -> getBindings e

typeCheck :: Expr -> State Context (Either FievelError Type)
typeCheck (ELam x expr) = do
  err <- getBindings expr
  case err of
    Just e -> return $ Left e
    Nothing -> do 
      t' <- typeCheck expr
      st <- get
      let t = M.lookup x st
      case (t, t') of
        (Just typ, Right typ') -> return . Right $ TLam typ typ'
        (Just _, Left err)     -> return $ Left err
        (Nothing, _)           -> return . Left $ TypeError $ "Variable " ++ show x ++ "not bound in expression " ++ show expr
