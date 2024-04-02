{-# LANGUAGE FlexibleInstances #-}

module Common.Utils where

import Parser.Abs

class MakeCompound a where makeCompoundAssignment :: BNFC'Position -> Expr -> a -> Expr -> Expr
instance MakeCompound MulOp where makeCompoundAssignment pos e1 op e2 = EAssign pos e1 (OpAssign pos) (EMul pos e1 op e2)
instance MakeCompound AddOp where makeCompoundAssignment pos e1 op e2 = EAssign pos e1 (OpAssign pos) (EAdd pos e1 op e2)
