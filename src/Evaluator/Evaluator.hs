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
    eval (Program pos _) = throwError $ NotImplementedGTException pos
