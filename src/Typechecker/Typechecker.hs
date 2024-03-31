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

ensureType :: BNFC'Position -> Expr -> TCType -> TypecheckM
ensureType pos e t = do
    et <- tcheck e
    unless (et == t) $ throwError $ WrongTypeGTException pos (show t) (show et)
    pure t

tcheckBuiltin :: BNFC'Position -> Expr -> [Expr] -> TypecheckM -> TypecheckM
tcheckBuiltin pos (EIdent _ (Ident "print")) args _ = mapM_ tcheck args >> pure TCVoid
tcheckBuiltin _ _ _ tm = tm

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
    tcheck (DConst _ x e) = tcheck e >>= \et -> modify (tsPut x (TCConst et, TSInitialized)) >> pure TCInt
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
    tcheck (IExpr _ e) = tcheck e
    tcheck (IIf pos e i) = ensureType pos e TCBool >> tcheck i
    tcheck (IIfElse pos e i1 i2) = ensureType pos e TCBool >> tcheck i1 >> tcheck i2
    tcheck (IWhile pos e i) = do
        ts <- get
        let inLoop = _inLoop ts
        void $ ensureType pos e TCBool
        put $ ts{_inLoop = True}
        void $ tcheck i
        put $ ts{_inLoop = inLoop}
        pure TCVoid
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
    tcheck (ELitString _ _) = pure $ TCArray TCChar
    tcheck (ELitTrue _) = pure TCBool
    tcheck (ELitFalse _) = pure TCBool
    tcheck (EIdent pos x) = do
        ts <- get
        case tsGet x ts of
            Just (t, TSInitialized) -> pure t
            Just (_, TSUninitialized) -> throwError $ UninitializedVariableGTException pos x
            Nothing -> throwError $ UndeclaredVariableGTException pos x
    tcheck (EIndex pos _ _) = throwError $ NotImplementedGTException pos
    tcheck (EApply pos f args) =
        tcheckBuiltin pos f args $
            tcheck f >>= \case
                TCFunc params ret -> do
                    let (nargs, nparams) = (length args, length params)
                    unless (nargs == nparams) $ throwError $ WrongNumberOfArgumentsGTException pos nparams nargs
                    let okArg :: (TCType, TCType, Expr) -> Bool
                        okArg (TCRef v1, v2, EIdent _ _) = dropQualifier v1 == v2
                        okArg (v1, v2, _) = dropQualifier v1 == v2
                    argTypes <- mapM tcheck args
                    unless (all okArg (zip3 params argTypes args)) $
                        throwError (WrongArgumentTypeGTException pos (show params) (show argTypes))
                    pure ret
                _ | otherwise -> throwError $ NotAFunctionGTException pos
    tcheck (EUOp pos (OpUnaryPlus _) e) = ensureType pos e TCInt
    tcheck (EUOp pos (OpUnaryMinus _) e) = ensureType pos e TCInt
    tcheck (EUOp pos (OpUnaryBang _) e) = ensureType pos e TCBool
    tcheck (EMul pos e1 _ e2) = ensureType pos e1 TCInt >> ensureType pos e2 TCInt
    tcheck (EAdd pos e1 _ e2) = ensureType pos e1 TCInt >> ensureType pos e2 TCInt
    tcheck (ERel pos e1 _ e2) = ensureType pos e1 TCInt >> ensureType pos e2 TCInt >> pure TCBool
    tcheck (EEq pos e1 _ e2) = tcheck e1 >>= ensureType pos e2 >> pure TCBool
    tcheck (EAnd pos e1 e2) = ensureType pos e1 TCBool >> ensureType pos e2 TCBool
    tcheck (EOr pos e1 e2) = ensureType pos e1 TCBool >> ensureType pos e2 TCBool
    tcheck (EAssign pos1 (EIdent pos2 x) _ e) = do
        ts <- get
        case tsGet x ts of
            Just (TCConst _, _) -> throwError $ AssignmentToReadOnlyVariable pos1 x
            Just (t, _) -> modify (tsPut x (t, TSInitialized)) >> ensureType pos1 e t
            Nothing -> throwError $ UndeclaredVariableGTException pos2 x
    -- tcheck (EAssign pos (EIndex _ (EIdent _ x) idx) _ e) = throwError $ NotImplementedGTException pos
    tcheck (EAssign pos _ _ _) = throwError $ NotImplementedGTException Nothing
    tcheck (ELambda pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (EEmpty _) = pure TCVoid
