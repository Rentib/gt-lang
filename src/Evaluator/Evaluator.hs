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

evalIfNoFlag :: EvalM -> EvalM
evalIfNoFlag m = do
    es <- get
    case flag es of
        ESFNone -> m
        _ | otherwise -> pure VVoid

instance Evaluator Instr where
    eval (IBlock _ block) = evalIfNoFlag $ eval block
    eval (IExpr _ e) = evalIfNoFlag $ eval e
    eval (IIf _ e i) = evalIfNoFlag $ do
        eval e >>= \v -> if v == VBool True then eval i else pure VVoid
    eval (IIfElse _ e i1 i2) = evalIfNoFlag $ do
        eval e >>= \v -> if v == VBool True then eval i1 else eval i2
    eval (IWhile pos e i) = evalIfNoFlag $ do
        v <- eval e
        if v == VBool True
            then do
                void $ eval i
                es <- get
                case flag es of
                    ESFContinue -> modify (esPutFlag ESFNone) >> eval (IWhile pos e i)
                    ESFBreak -> modify (esPutFlag ESFNone) >> pure VVoid
                    _ | otherwise -> eval (IWhile pos e i)
            else pure VVoid
    eval (IDo pos i e) = evalIfNoFlag $ eval i >> eval (IWhile pos e i)
    eval (IFor pos e1 e2 e3 i) =
        evalIfNoFlag $
            eval e1 >> eval (IWhile pos e2 (IBlock pos (PBlock pos [] [i, IExpr pos e3])))
    eval (IContinue _) = evalIfNoFlag $ modify (esPutFlag ESFContinue) >> pure VVoid
    eval (IBreak _) = evalIfNoFlag $ modify (esPutFlag ESFBreak) >> pure VVoid
    eval (IReturn _ e) = evalIfNoFlag $ eval e >>= \v -> modify (esPutFlag $ ESFReturn v) >> pure VVoid

instance Evaluator Expr where
    eval (ELitInt _ n) = pure $ VInt n
    eval (ELitChar pos c) = throwError $ NotImplementedGTException pos
    eval (ELitTrue _) = pure $ VBool True
    eval (ELitFalse _) = pure $ VBool False
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
    eval (ERel pos e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        let cmp f (VInt n1) (VInt n2) = VBool $ f n1 n2
            cmp _ _ _ = undefined
        case op of
            OpLT _ -> pure $ cmp (<) v1 v2
            OpLE _ -> pure $ cmp (<=) v1 v2
            OpGT _ -> pure $ cmp (>) v1 v2
            OpGE _ -> pure $ cmp (>=) v1 v2
    eval (EEq pos e1 op e2) = throwError $ NotImplementedGTException pos
    eval (EAnd pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EOr pos e1 e2) = throwError $ NotImplementedGTException pos
    eval (EAssign pos e1 _ e2) = do
        v2 <- eval e2
        case e1 of
            EIdent _ x -> do
                modify $ esUpdate x v2
                pure v2
            -- TODO: arrays
            _ | otherwise -> throwError $ NotImplementedGTException pos
    eval (ELambda pos args t block) = throwError $ NotImplementedGTException pos
    eval (EEmpty _) = pure VVoid
