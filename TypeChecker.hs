{-# LANGUAGE MultiWayIf #-}
module TypeChecker where

import Types

--   EVal Value
-- | EVar String           -- Variable
-- | EOp PrimOp            -- primitive operator
-- | EIf Expr Expr Expr    -- if then else
-- | ELet String Expr Expr -- let..in (substitution)
-- | ELam String Expr      -- \x -> e (lambda abstraction)
-- | EAp Expr Expr         -- function application
-- | EDef String Expr      -- Binding
-- | EType String Type     -- type signature

typeOf :: Expr -> Either FievelError Type
typeOf (EVal (VInt _))  = Right TInt
typeOf (EVal (VBool _)) = Right TBool
typeOf (EVal (VStr _))  = Right TStr
typeOf (EOp op)         = typeOfOp op
typeOf (EIf e1 e2 e3)   = typeOfIf e1 e2 e3
typeOf (EVar s)         = Left . TypeError $ "Variable not bound: " ++ s
typeOf _                = Left . TypeError $ "Not yet implemented."

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
typeOfOp op = case op of
  NNBO nnbo -> checkNNBOType nnbo
  SSBO ssbo -> checkSSBOType ssbo
  NBBO nbbo -> checkNBBOType nbbo
  BUO  buo  -> checkBUOType  buo
  BBO  bbo  -> checkBBOType  bbo

-- Check a binary operator for well-typedness; throw type error if ill-typed.
checkBinOp :: Expr -> Expr -> Type -> Type -> Type -> String -> Either FievelError Type
checkBinOp e1 e2 et1 et2 ft opname = 
  let t1 = typeOf e1
      t2 = typeOf e2
  in case (t1, t2) of
    (Right t, Right t') -> 
      if | t == et1 && t' == et2 -> Right ft
         | t  /= et1 -> Left . TypeError $ "Expected " ++ show et1 ++ " but got " ++ show t ++ " in the first argument of " ++ opname ++ ", namely " ++ show e1
         | t' /= et2 -> Left . TypeError $ "Expected " ++ show et2 ++ " but got " ++ show t' ++ " in the second argument of " ++ opname ++ ", namely " ++ show e2
    (l@(Left t), _) -> l
    (_, l@(Left t)) -> l

checkUnaryOp :: Expr -> Type -> Type -> String -> Either FievelError Type
checkUnaryOp e et ft opname = 
  let t = typeOf e
  in case t of
    Right t'   -> if t' == et 
                  then Right ft 
                  else Left . TypeError $ "Expected " ++ show et ++ " but got " ++ show t' ++ " in the only argument of " ++ opname ++ ", namely " ++ show e 
    l@(Left _) -> l

checkBBOType :: BoolBinOp -> Either FievelError Type
checkBBOType e = 
  let chk e1 e2 = checkBinOp e1 e2 TBool TBool TBool
  in case e of
    e1 :|: e2 -> chk e1 e2 "|"
    e1 :&: e2 -> chk e1 e2 "&"

checkBUOType ::  BoolUnaryOp -> Either FievelError Type
checkBUOType e = 
  let chk e' = checkUnaryOp e' TBool TBool
  in case e of
    BNot ex -> chk ex "!"

checkNBBOType :: NumBoolBinOp -> Either FievelError Type 
checkNBBOType e = 
  let chk e1 e2 = checkBinOp e1 e2 TInt TInt TBool
  in case e of 
      e1 :=:  e2 -> chk e1 e2 "="
      e1 :!=: e2 -> chk e1 e2 "!="
      e1 :<:  e2 -> chk e1 e2 "<"
      e1 :>:  e2 -> chk e1 e2 ">"
      e1 :>=: e2 -> chk e1 e2 ">="
      e1 :<=: e2 -> chk e1 e2 "<="

checkSSBOType :: StringStringBinOp -> Either FievelError Type 
checkSSBOType e = 
  let chk e1 e2 = checkBinOp e1 e2 TStr TStr TStr
  in case e of
    e1 :<>: e2 -> chk e1 e2 "<>"

checkNNBOType :: NumNumBinOp -> Either FievelError Type 
checkNNBOType e = 
  let chk e1 e2 = checkBinOp e1 e2 TInt TInt TInt
  in case e of
      e1 :+: e2 -> chk e1 e2 "+"
      e1 :-: e2 -> chk e1 e2 "-"
      e1 :*: e2 -> chk e1 e2 "*"
      e1 :/: e2 -> chk e1 e2 "/"

