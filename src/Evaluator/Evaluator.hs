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
    eval (PBlock _ decls instrs) = do
        es <- get
        mapM_ eval decls >> mapM_ eval instrs
        es' <- get
        put $ es'{env = env es}
        pure VVoid

instance Evaluator Decl where
    eval (DNoInit _ x _) = do
        modify $ esNew x VUninitialized
        pure VVoid
    eval (DInit _ x e) = do
        v <- eval e
        modify $ esNew x v
        pure VVoid
    eval (DConst _ x e) = do
        v <- eval e
        modify $ esNew x v
        pure VVoid
    eval (DFunc _ f args _ block) = do
        es <- get
        modify $ esNew f (VFunc args block (env es))
        pure VVoid

instance Evaluator Instr where
    eval (IBlock _ block) = eval block
    eval (IExpr pos e) = throwError $ NotImplementedGTException pos
    eval (IIf pos e i) = throwError $ NotImplementedGTException pos
    eval (IIfElse pos e i1 i2) = throwError $ NotImplementedGTException pos
    eval (IWhile pos e i) = throwError $ NotImplementedGTException pos
    eval (IDo pos i e) = throwError $ NotImplementedGTException pos
    eval (IFor pos e1 e2 e3 i) = throwError $ NotImplementedGTException pos
    eval (IContinue pos) = throwError $ NotImplementedGTException pos
    eval (IBreak pos) = throwError $ NotImplementedGTException pos
    eval (IReturn _ e) = do
        es <- get
        v <- eval e
        put $ es{flag = ESFReturn v}
        pure VVoid

instance Evaluator Expr where
    eval (ELitInt _ n) = pure $ VInt n
    eval (ELitChar pos c) = throwError $ NotImplementedGTException pos
    eval (ELitTrue pos) = throwError $ NotImplementedGTException pos
    eval (ELitFalse pos) = throwError $ NotImplementedGTException pos
    eval (EIdent _ x) = gets $ esGet x
    eval (EIndex pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EApply pos e args) = do
        -- TODO: implement properly
        f <- eval e
        (VFunc _ block _) <- case f of
            VFunc{} -> pure f
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
        void $ eval block
        es <- get
        pure $ case flag es of
            ESFReturn v -> v
            _ | otherwise -> VVoid
    eval (EUOp pos op e) = throwError $ NotImplementedGTException pos
    eval (EMul pos e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VInt n1, VInt n2) -> case op of
                OpTimes _ -> pure $ VInt $ n1 * n2
                OpDiv pos' -> do
                    when (n2 == 0) $ throwError $ DivideByZeroGTException pos'
                    pure $ VInt $ n1 `div` n2
                OpMod pos' -> do
                    when (n2 == 0) $ throwError $ DivideByZeroGTException pos'
                    pure $ VInt $ n1 `mod` n2
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EAdd pos e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VInt n1, VInt n2) -> case op of
                OpPlus _ -> pure $ VInt $ n1 + n2
                OpMinus _ -> pure $ VInt $ n1 - n2
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (ERel pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (EEq pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (EAnd pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EOr pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EAssign pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (ELambda pos args t block) = throwError $ NotImplementedGTException pos
    eval (EEmpty pos) = throwError $ NotImplementedGTException pos
