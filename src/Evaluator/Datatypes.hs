{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator.Datatypes where

import Data.Array
import Data.Map as Map

import Parser.Abs

data Value where
    VInt :: Integer -> Value
    VBool :: Bool -> Value
    VChar :: Char -> Value
    VVoid :: Value
    VFunc :: [Arg] -> Block -> Env -> Value
    VArray :: Array Integer Value -> Value

instance Eq Value where
    VInt i == VInt j = i == j
    VBool b == VBool c = b == c
    VChar c == VChar d = c == d
    VVoid == VVoid = True
    VFunc{} == VFunc{} = False
    VArray a == VArray b = a == b
    _ == _ = False

instance Show Value where
    show (VInt i) = show i
    show (VBool b) = show b
    show (VChar c) = show c
    show VVoid = "<void>"
    show (VFunc{}) = "<function>"
    show (VArray a) = show a

data EvaluatorStateFlag where
    ESFNone :: EvaluatorStateFlag
    ESFReturn :: EvaluatorStateFlag
    ESFContinue :: EvaluatorStateFlag
    ESFBreak :: EvaluatorStateFlag

type Loc = Integer
data Env where Env :: {_env :: Map Ident Loc} -> Env
data Store where Store :: {_store :: Map Loc Value, _newloc :: Loc} -> Store
data EvaluatorState where
    EvaluatorState ::
        { env :: Env
        , store :: Store
        , flag :: EvaluatorStateFlag
        } ->
        EvaluatorState

envEmpty :: Env
envEmpty = Env Map.empty

envPut :: Ident -> Loc -> Env -> Env
envPut x l (Env e) = Env $ Map.insert x l e

envGet :: Ident -> Env -> Loc
envGet x (Env e) = e Map.! x

storeEmpty :: Store
storeEmpty = Store Map.empty 0

storePut :: Loc -> Value -> Store -> Store
storePut l v (Store s n) = Store (Map.insert l v s) n

storeGet :: Loc -> Store -> Value
storeGet l (Store s _) = s Map.! l

storeNewLoc :: Store -> (Store, Loc)
storeNewLoc (Store s l) = (Store s (l + 1), l)

esEmpty :: EvaluatorState
esEmpty = EvaluatorState envEmpty storeEmpty ESFNone

esNew :: Ident -> Value -> EvaluatorState -> EvaluatorState
esNew x v EvaluatorState{..} = EvaluatorState env' store' flag
  where
    (store'', l) = storeNewLoc store
    env' = envPut x l env
    store' = storePut l v store''

esUpdate :: Ident -> Value -> EvaluatorState -> EvaluatorState
esUpdate x v EvaluatorState{..} = EvaluatorState env store' flag
  where
    l = envGet x env
    store' = storePut l v store

esGet :: Ident -> EvaluatorState -> Value
esGet x EvaluatorState{..} = storeGet (envGet x env) store
