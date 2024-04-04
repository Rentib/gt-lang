{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Typechecker.Datatypes where

import Common.EnvStore

import Data.Maybe
import Parser.Abs

data TCType where
    TCInt :: TCType
    TCBool :: TCType
    TCChar :: TCType
    TCVoid :: TCType
    TCFunc :: [TCType] -> TCType -> TCType
    TCArray :: TCType -> TCType
    TCConst :: TCType -> TCType
    TCRef :: TCType -> TCType
    deriving (Eq)

instance Show TCType where
    show TCInt = "int"
    show TCBool = "bool"
    show TCChar = "char"
    show TCVoid = "void"
    show (TCFunc args ret) = "(" ++ unwords (map show args) ++ ") -> " ++ show ret
    show (TCArray t) = show t ++ "[]"
    show (TCConst t) = "const " ++ show t
    show (TCRef t) = show t ++ "&"

fromType :: Type -> TCType
fromType (TInt _) = TCInt
fromType (TBool _) = TCBool
fromType (TChar _) = TCChar
fromType (TVoid _) = TCVoid
fromType (TFunc _ args ret) = TCFunc (map fromType args) (fromType ret)
fromType (TArray _ t) = TCArray $ fromType t
fromType (TConst _ t) = TCConst $ fromType t

fromFunction :: [Arg] -> Type -> TCType
fromFunction args ret = TCFunc (map fromArg args) (fromType ret)

fromArg :: Arg -> TCType
fromArg (PArgVal _ _ t) = fromType t
fromArg (PArgRef _ _ t) = TCRef (fromType t)

dropQualifier :: TCType -> TCType
dropQualifier (TCRef t) = TCRef $ dropQualifier t
dropQualifier (TCConst t) = t
dropQualifier t = t

isCastable :: TCType -> TCType -> Bool
isCastable t1 t2 = isCastable' (dropQualifier t1) (dropQualifier t2)

isCastable' :: TCType -> TCType -> Bool
isCastable' (TCArray t1) (TCArray t2) = isCastable' t1 t2
isCastable' (TCArray _) _ = False
isCastable' _ (TCArray _) = False
isCastable' (TCRef t1) (TCRef t2) = isCastable' t1 t2
isCastable' (TCRef _) _ = False
isCastable' _ (TCRef _) = False
isCastable' (TCFunc _ _) _ = False
isCastable' _ (TCFunc _ _) = False
isCastable' _ _ = True

data TCState where
    TSInitialized :: TCState
    TSUninitialized :: TCState
    deriving (Show, Eq)

type TCValue = (TCType, TCState)

data TypecheckerState where
    TypecheckerState ::
        { env :: Env
        , store :: Store TCValue
        , _retType :: TCType
        , _hasReturn :: Bool
        , _inLoop :: Bool
        } ->
        TypecheckerState

tsEmpty :: TypecheckerState
tsEmpty = TypecheckerState envEmpty storeEmpty TCVoid False False

tsNew :: Ident -> TCValue -> TypecheckerState -> TypecheckerState
tsNew x v TypecheckerState{..} = TypecheckerState{env = env', store = store', ..}
  where
    (store'', l) = storeNewLoc store
    env' = envPut x l env
    store' = storePut l v store''

tsUpdate :: Ident -> TCValue -> TypecheckerState -> TypecheckerState
tsUpdate x v TypecheckerState{..} = TypecheckerState{env = env, store = store', ..}
  where
    l = fromJust $ envGet x env
    store' = storePut l v store

tsGet :: Ident -> TypecheckerState -> Maybe TCValue
tsGet x TypecheckerState{..} = case envGet x env of
    Just l -> storeGet l store
    Nothing -> Nothing
