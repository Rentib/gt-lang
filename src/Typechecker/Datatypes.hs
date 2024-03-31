{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Typechecker.Datatypes where

import Data.Map as Map (Map, empty, insert, lookup)

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
    TCGeneric :: TCType

instance Eq TCType where
    TCInt == TCInt = True
    TCBool == TCBool = True
    TCChar == TCChar = True
    TCVoid == TCVoid = True
    TCFunc args1 ret1 == TCFunc args2 ret2 = args1 == args2 && ret1 == ret2
    TCArray t1 == TCArray t2 = t1 == t2
    TCConst t1 == TCConst t2 = t1 == t2
    TCRef t1 == TCRef t2 = t1 == t2
    TCGeneric == _ = True
    _ == TCGeneric = True
    _ == _ = False

instance Show TCType where
    show TCInt = "int"
    show TCBool = "bool"
    show TCChar = "char"
    show TCVoid = "void"
    show (TCFunc args ret) = "(" ++ unwords (map show args) ++ ") -> " ++ show ret
    show (TCArray t) = show t ++ "[]"
    show (TCConst t) = "const " ++ show t
    show (TCRef t) = show t ++ "&"
    show TCGeneric = "generic"

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

data TCState where
    TSInitialized :: TCState
    TSUninitialized :: TCState
    deriving (Show, Eq)

type TCValue = (TCType, TCState)

data Env where
    Env ::
        { _env :: Map.Map Ident TCValue
        , _retType :: TCType
        , _hasReturn :: Bool
        , _inLoop :: Bool
        } ->
        Env

type TypecheckerState = Env

tsEmpty :: Env
tsEmpty = Env Map.empty TCVoid False False

tsPut :: Ident -> TCValue -> Env -> Env
tsPut x v Env{..} = Env{_env = Map.insert x v _env, ..}
tsGet :: Ident -> Env -> Maybe TCValue
tsGet x Env{..} = Map.lookup x _env
