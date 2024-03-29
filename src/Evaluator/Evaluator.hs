{-# LANGUAGE FlexibleInstances #-}

module Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.State

import Common.Exceptions
import Evaluator.Datatypes
import Parser.Abs

evaluate :: TranslationUnit -> IO (Either GTException Value)
evaluate prog = evalStateT (runExceptT (eval prog)) esEmpty

type EvalM = EvalM' Value
type EvalM' a = ExceptT GTException (StateT EvaluatorState IO) a
class Evaluator a where
    eval :: a -> EvalM

instance Evaluator TranslationUnit where
    eval (Program pos decls) =
        mapM_ eval decls >> eval (EApply pos (EIdent pos (Ident "main")) [])

instance Evaluator Block where
    eval (PBlock pos decls instrs) = throwError $ NotImplementedGTException pos

instance Evaluator Decl where
    eval (DNoInit pos x t) = throwError $ NotImplementedGTException pos
    eval (DInit pos x e) = throwError $ NotImplementedGTException pos
    eval (DConst pos x e) = throwError $ NotImplementedGTException pos
    eval (DFunc pos x args t block) = throwError $ NotImplementedGTException pos

instance Evaluator Instr where
    eval (IBlock pos block) = throwError $ NotImplementedGTException pos
    eval (IExpr pos e) = throwError $ NotImplementedGTException pos
    eval (IIf pos e i) = throwError $ NotImplementedGTException pos
    eval (IIfElse pos e i1 i2) = throwError $ NotImplementedGTException pos
    eval (IWhile pos e i) = throwError $ NotImplementedGTException pos
    eval (IDo pos i e) = throwError $ NotImplementedGTException pos
    eval (IFor pos e1 e2 e3 i) = throwError $ NotImplementedGTException pos
    eval (IContinue pos) = throwError $ NotImplementedGTException pos
    eval (IBreak pos) = throwError $ NotImplementedGTException pos
    eval (IReturn pos e) = throwError $ NotImplementedGTException pos

instance Evaluator Expr where
    eval (ELitInt pos n) = throwError $ NotImplementedGTException pos
    eval (ELitChar pos c) = throwError $ NotImplementedGTException pos
    eval (ELitTrue pos) = throwError $ NotImplementedGTException pos
    eval (ELitFalse pos) = throwError $ NotImplementedGTException pos
    eval (EIdent pos x) = throwError $ NotImplementedGTException pos
    eval (EIndex pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EApply pos e es) = throwError $ NotImplementedGTException pos
    eval (EUOp pos op e) = throwError $ NotImplementedGTException pos
    eval (EMul pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (EAdd pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (ERel pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (EEq pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (EAnd pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EOr pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EAssign pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (ELambda pos args t block) = throwError $ NotImplementedGTException pos
    eval (EEmpty pos) = throwError $ NotImplementedGTException pos
