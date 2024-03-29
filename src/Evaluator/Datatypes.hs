{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator.Datatypes where

import Data.Array
import Data.Map as Map

import Parser.Abs

data Value where
    EvInt :: Integer -> Value
    EvBool :: Bool -> Value
    EvChar :: Char -> Value
    EvVoid :: Value
    EvFunc :: [Arg] -> Block -> Env -> Value
    EvArray :: Array Integer Value -> Value

instance Eq Value where
    EvInt i == EvInt j = i == j
    EvBool b == EvBool c = b == c
    EvChar c == EvChar d = c == d
    EvVoid == EvVoid = True
    EvFunc{} == EvFunc{} = False
    EvArray a == EvArray b = a == b
    _ == _ = False

instance Show Value where
    show (EvInt i) = show i
    show (EvBool b) = show b
    show (EvChar c) = show c
    show EvVoid = "<void>"
    show (EvFunc{}) = "<function>"
    show (EvArray a) = show a

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
