{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Typechecker.Typechecker where

import Control.Monad.Except
import Control.Monad.State

import Parser.Abs

import Common.Exceptions
import Typechecker.Datatypes

typecheck :: TranslationUnit -> Either GTException ()
typecheck tu = void $ runExcept $ evalStateT (tcheck tu) tsEmpty

type TypecheckM = TypecheckM' TCType
type TypecheckM' a = StateT TypecheckerState (Except GTException) a
class Typechecker a where
    tcheck :: a -> TypecheckM

instance Typechecker TranslationUnit where
    tcheck (Program pos decls) = do
        mapM_ tcheck decls >> get >>= \ts -> case tsGet (Ident "main") ts of
            Just (TCFunc [] TCInt, TSInitialized) -> pure TCInt
            Just (TCFunc [] t, TSInitialized) -> throwError $ WrongTypeGTException pos (show TCInt) (show t)
            Just (TCFunc args _, _) -> throwError $ WrongNumberOfArgumentsGTException pos (length args) 0
            _ | otherwise -> throwError $ MissingMainGTException pos

instance Typechecker Block where
    tcheck (PBlock _ decls instrs) = do
        ts <- get
        mapM_ tcheck decls
        mapM_ tcheck instrs
        ts' <- get
        put $ ts'{_env = _env ts}
        pure TCInt

instance Typechecker Decl where
    tcheck (DNoInit _ x t) = modify (tsPut x (fromType t, TSUninitialized)) >> pure TCInt
    tcheck (DInit _ x e) = tcheck e >>= \et -> modify (tsPut x (et, TSInitialized)) >> pure TCInt
    tcheck (DConst pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (DFunc pos f args ret block) = do
        modify $ tsPut f (fromFunction args ret, TSInitialized)
        ts <- get
        put $ ts{_retType = fromType ret, _hasReturn = False, _inLoop = False}
        let addArg (PArgVal _ x t) = tsPut x (fromType t, TSInitialized)
            addArg (PArgRef _ x t) = tsPut x (fromType t, TSInitialized)
        mapM_ (modify . addArg) args
        void $ tcheck block
        ts' <- get
        unless (_hasReturn ts' || fromType ret == TCVoid) $ throwError $ FunctionWithoutReturnGTException pos (show f)
        put ts
        pure TCInt

instance Typechecker Instr where
    tcheck (IBlock _ block) = tcheck block
    tcheck (IExpr pos _) = throwError $ NotImplementedGTException pos
    tcheck (IIf pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (IIfElse pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (IWhile pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (IFor pos _ _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (IContinue pos) = throwError $ NotImplementedGTException pos
    tcheck (IBreak pos) = throwError $ NotImplementedGTException pos
    tcheck (IReturn pos e) = do
        ts <- get
        t <- tcheck e
        unless (t == _retType ts) $ throwError $ WrongReturnTypeGTException pos (show $ _retType ts) (show t)
        put (ts{_hasReturn = True}) >> pure TCInt

instance Typechecker Expr where
    tcheck (ELitInt _ _) = pure TCInt
    tcheck (ELitChar _ _) = pure TCChar
    tcheck (ELitString pos _) = throwError $ NotImplementedGTException pos
    tcheck (ELitTrue _) = pure TCBool
    tcheck (ELitFalse _) = pure TCBool
    tcheck (EIdent pos _) = throwError $ NotImplementedGTException pos
    tcheck (EIndex pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (EApply pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (EUOp pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (EMul pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (EAdd pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (ERel pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (EEq pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (EAnd pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (EOr pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (EAssign pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (ELambda pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (EEmpty pos) = throwError $ NotImplementedGTException pos
