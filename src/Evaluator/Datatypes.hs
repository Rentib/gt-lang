{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluator.Datatypes where

import Common.EnvStore
import Data.Array

import Data.Maybe
import Parser.Abs

data Value where
    VInt :: Integer -> Value
    VBool :: Bool -> Value
    VChar :: Char -> Value
    VVoid :: Value
    VFunc :: Ident -> [Arg] -> Block -> Env -> Value
    VArray :: Array Integer Value -> Value
    VUninitialized :: Value

instance Eq Value where
    VInt i == VInt j = i == j
    VBool b == VBool c = b == c
    VChar c == VChar d = c == d
    VVoid == VVoid = True
    VFunc{} == VFunc{} = False
    VArray a == VArray b = a == b
    _ == _ = False

instance Ord Value where
    VInt i <= VInt j = i <= j
    VBool b <= VBool c = b <= c
    VChar c <= VChar d = c <= d
    VVoid <= VVoid = True
    VFunc{} <= VFunc{} = False
    VArray a <= VArray b = a <= b
    _ <= _ = False

instance Show Value where
    show (VInt i) = show i
    show (VBool b) = show b
    show (VChar c) = [c]
    show VVoid = "<void>"
    show (VFunc{}) = "<function>"
    show (VArray a) =
        let e = Data.Array.elems a
            isChar (VChar _) = True
            isChar _ = False
         in if all isChar e
                then concatMap show e
                else show e
    show VUninitialized = "<uninitialized>"

data EvaluatorStateFlag where
    ESFNone :: EvaluatorStateFlag
    ESFReturn :: Value -> EvaluatorStateFlag
    ESFContinue :: EvaluatorStateFlag
    ESFBreak :: EvaluatorStateFlag

data EvaluatorState where
    EvaluatorState ::
        { env :: Env
        , store :: Store Value
        , flag :: EvaluatorStateFlag
        } ->
        EvaluatorState

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
    l = fromJust $ envGet x env
    store' = storePut l v store

esGet :: Ident -> EvaluatorState -> Value
esGet x EvaluatorState{..} = fromJust $ storeGet (fromJust (envGet x env)) store

esGetFlag :: EvaluatorState -> EvaluatorStateFlag
esGetFlag EvaluatorState{..} = flag

esPutFlag :: EvaluatorStateFlag -> EvaluatorState -> EvaluatorState
esPutFlag flag' EvaluatorState{..} = EvaluatorState env store flag'
