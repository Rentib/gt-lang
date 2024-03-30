{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Common.Exceptions where

import Parser.Abs

type GTException = GTException' BNFC'Position
data GTException' a where
    NotImplementedGTException :: a -> GTException' a
    UnknownRuntimeGTException :: a -> GTException' a
    DivideByZeroGTException :: a -> GTException' a
    MallocFailedGTException :: a -> GTException' a

instance Show GTException where
    show (NotImplementedGTException pos) = "Not implemented at " ++ showpos pos
    show (UnknownRuntimeGTException pos) = "Unknown runtime exception at " ++ showpos pos
    show (DivideByZeroGTException pos) = "Division by zero at " ++ showpos pos
    show (MallocFailedGTException pos) = "Malloc failed at " ++ showpos pos

showpos :: BNFC'Position -> String
showpos (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c
showpos Nothing = "unknown position"
