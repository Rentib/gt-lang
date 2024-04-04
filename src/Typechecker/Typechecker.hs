{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Typechecker.Typechecker where

import Control.Monad.Except
import Control.Monad.State

import Parser.Abs

import Common.Exceptions
import Common.Utils
import Typechecker.Datatypes

typecheck :: TranslationUnit -> Either GTException ()
typecheck tu = void $ runExcept $ evalStateT (tcheck tu) tsEmpty

type TypecheckM = TypecheckM' TCType
type TypecheckM' a = StateT TypecheckerState (Except GTException) a
class Typechecker a where
    tcheck :: a -> TypecheckM

ensureType :: BNFC'Position -> Expr -> TCType -> TypecheckM
ensureType pos e t =
    tcheck e >>= \et -> unless (et == t) (throwError $ WrongTypeGTException pos (show t) (show et)) >> pure t

tcheckBuiltin :: BNFC'Position -> Expr -> [Expr] -> TypecheckM -> TypecheckM
tcheckBuiltin _ (EIdent _ (Ident "print")) args _ = mapM_ tcheck args >> pure TCVoid
tcheckBuiltin pos (EIdent _ (Ident "malloc")) [n] _ = void (ensureType pos n TCInt) >> pure (TCArray TCVoid)
tcheckBuiltin pos (EIdent _ (Ident "malloc")) args _ =
    throwError $ WrongNumberOfArgumentsGTException pos 1 (length args)
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
        mapM_ tcheck decls >> mapM_ tcheck instrs
        ts' <- get
        put $ ts'{env = env ts}
        pure TCInt

instance Typechecker Decl where
    tcheck (DVar _ xs) = do
        let addItem :: DItem -> TypecheckM
            addItem (DItemNoInit _ x t) = modify (tsNew x (fromType t, TSUninitialized)) >> pure TCInt
            addItem (DItemInit _ x e) = tcheck e >>= \et -> modify (tsNew x (et, TSInitialized)) >> pure TCInt
        mapM_ addItem xs >> pure TCVoid
    tcheck (DConst _ xs) = do
        let addItem :: DItemConst -> TypecheckM
            addItem (DItemConstInit _ x e) = tcheck e >>= \et -> modify (tsNew x (TCConst et, TSInitialized)) >> pure TCInt
        mapM_ addItem xs >> pure TCVoid
    tcheck (DFunc pos f args ret block) = do
        modify $ tsNew f (fromFunction args ret, TSInitialized)
        ts <- get
        put $ ts{_retType = fromType ret, _hasReturn = False, _inLoop = False}
        let addArg (PArgVal _ x t) = tsNew x (fromType t, TSInitialized)
            addArg (PArgRef _ x t) = tsNew x (fromType t, TSInitialized)
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
        put $ ts{_inLoop = True}
        void $ ensureType pos e TCBool >> tcheck i
        put $ ts{_inLoop = inLoop}
        pure TCVoid
    tcheck (IFor pos e1 e2 e3 i) = tcheck e1 >> tcheck (IWhile pos e2 i) >> tcheck e3
    tcheck (IContinue pos) = do
        inLoop <- gets _inLoop
        unless inLoop $ throwError $ ContinueOutsideLoopGTException pos
        pure TCVoid
    tcheck (IBreak pos) = do
        inLoop <- gets _inLoop
        unless inLoop $ throwError $ BreakOutsideLoopGTException pos
        pure TCVoid
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
            Just (t, TSInitialized) -> pure $ dropQualifier t
            Just (_, TSUninitialized) -> throwError $ UninitializedVariableGTException pos x
            Nothing -> throwError $ UndeclaredVariableGTException pos x
    tcheck (EIndex pos e1 e2) = do
        tcheck e2 >> tcheck e1 >>= \case
            TCArray t -> ensureType pos e2 TCInt >> pure t
            _ | otherwise -> throwError $ NotAnArrayGTException pos
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
    tcheck (ECast pos t e) = do
        et <- tcheck e
        let tt = fromType t
        unless (isCastable et tt) $ throwError $ InvalidCastGTException pos (show et) (show tt)
        pure tt
    tcheck (EUOp pos (OpUnaryPlus _) e) = ensureType pos e TCInt
    tcheck (EUOp pos (OpUnaryMinus _) e) = ensureType pos e TCInt
    tcheck (EUOp pos (OpUnaryBang _) e) = ensureType pos e TCBool
    tcheck (EUOp pos (OpUnaryInc _) e) = ensureType pos e TCInt
    tcheck (EUOp pos (OpUnaryDec _) e) = ensureType pos e TCInt
    tcheck (EMul pos e1 _ e2) = ensureType pos e1 TCInt >> ensureType pos e2 TCInt
    tcheck (EAdd pos e1 _ e2) = ensureType pos e1 TCInt >> ensureType pos e2 TCInt
    tcheck (ERel pos e1 _ e2) = ensureType pos e1 TCInt >> ensureType pos e2 TCInt >> pure TCBool
    tcheck (EEq pos e1 _ e2) = tcheck e1 >>= ensureType pos e2 >> pure TCBool
    tcheck (EAnd pos e1 e2) = ensureType pos e1 TCBool >> ensureType pos e2 TCBool
    tcheck (EOr pos e1 e2) = ensureType pos e1 TCBool >> ensureType pos e2 TCBool
    tcheck (EAssign pos1 (EIdent pos2 x) (OpAssign _) e) = do
        ts <- get
        case tsGet x ts of -- NOTE: dont use tcheck, as it drops qualifiers
            Just (TCConst _, _) -> throwError $ AssignmentToReadOnlyVariable pos1 x
            Just (t, _) -> modify (tsUpdate x (t, TSInitialized)) >> ensureType pos1 e t
            Nothing -> throwError $ UndeclaredVariableGTException pos2 x
    tcheck (EAssign pos (EIndex _ (EIdent _ x) idx) (OpAssign _) e) = do
        ts <- get
        case tsGet x ts of
            Just (TCArray t, _) -> do
                void $ ensureType pos idx TCInt
                modify (tsUpdate x (TCArray t, TSInitialized))
                ensureType pos e t
            Just _ -> throwError $ NotAnArrayGTException pos
            Nothing -> throwError $ UndeclaredVariableGTException pos x
    tcheck (EAssign pos1 e1 (OpAssignTimes pos2) e2) = tcheck $ makeCompoundAssignment pos1 e1 (OpTimes pos2) e2
    tcheck (EAssign pos1 e1 (OpAssignDiv pos2) e2) = tcheck $ makeCompoundAssignment pos1 e1 (OpDiv pos2) e2
    tcheck (EAssign pos1 e1 (OpAssignMod pos2) e2) = tcheck $ makeCompoundAssignment pos1 e1 (OpMod pos2) e2
    tcheck (EAssign pos1 e1 (OpAssignPlus pos2) e2) = tcheck $ makeCompoundAssignment pos1 e1 (OpPlus pos2) e2
    tcheck (EAssign pos1 e1 (OpAssignMinus pos2) e2) = tcheck $ makeCompoundAssignment pos1 e1 (OpMinus pos2) e2
    tcheck (EAssign pos _ _ _) = throwError $ NotImplementedGTException pos
    tcheck (ELambda pos params ret block) = do
        let f = DFunc pos (Ident "") params ret block
        void $ tcheck f
        pure $ fromFunction params ret
    tcheck (EEmpty _) = pure TCVoid
