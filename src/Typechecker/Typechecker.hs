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
    tcheck (PBlock pos _ _) = throwError $ NotImplementedGTException pos

instance Typechecker Decl where
    tcheck (DNoInit pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (DInit pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (DConst pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (DFunc pos f args ret block) = do
        modify $ tsPut f (fromFunction args ret, TSInitialized)
        pure TCInt

instance Typechecker Instr where
    tcheck (IBlock pos _) = throwError $ NotImplementedGTException pos
    tcheck (IExpr pos _) = throwError $ NotImplementedGTException pos
    tcheck (IIf pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (IIfElse pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (IWhile pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (IFor pos _ _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (IContinue pos) = throwError $ NotImplementedGTException pos
    tcheck (IBreak pos) = throwError $ NotImplementedGTException pos
    tcheck (IReturn pos _) = throwError $ NotImplementedGTException pos

instance Typechecker Expr where
    tcheck (ELitInt pos _) = throwError $ NotImplementedGTException pos
    tcheck (ELitChar pos _) = throwError $ NotImplementedGTException pos
    tcheck (ELitString pos _) = throwError $ NotImplementedGTException pos
    tcheck (ELitTrue pos) = throwError $ NotImplementedGTException pos
    tcheck (ELitFalse pos) = throwError $ NotImplementedGTException pos
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
