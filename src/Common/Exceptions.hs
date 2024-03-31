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
    IndexOutOfBoundsGTException :: a -> Integer -> GTException' a
    UnknownPreprocessorGTException :: a -> GTException' a
    MissingMainGTException :: a -> GTException' a
    WrongTypeGTException :: a -> String -> String -> GTException' a
    WrongNumberOfArgumentsGTException :: a -> Int -> Int -> GTException' a
    FunctionWithoutReturnGTException :: a -> String -> GTException' a
    WrongReturnTypeGTException :: a -> String -> String -> GTException' a

instance Show GTException where
    show (NotImplementedGTException pos) = "Not implemented at " ++ showpos pos
    show (UnknownRuntimeGTException pos) = "Unknown runtime exception at " ++ showpos pos
    show (DivideByZeroGTException pos) = "Division by zero at " ++ showpos pos
    show (MallocFailedGTException pos) = "Malloc failed at " ++ showpos pos
    show (IndexOutOfBoundsGTException pos idx) = "Index out of bounds at " ++ showpos pos ++ " (index: " ++ show idx ++ ")"
    show (UnknownPreprocessorGTException pos) = "Unknown preprocessor exception at " ++ showpos pos
    show (MissingMainGTException _) = "Missing main function"
    show (WrongTypeGTException pos expected got) = "Wrong type at " ++ showpos pos ++ ", expected " ++ expected ++ ", got " ++ got
    show (WrongNumberOfArgumentsGTException pos expected got) = "Wrong number of arguments at " ++ showpos pos ++ ", expected " ++ show expected ++ ", got " ++ show got
    show (FunctionWithoutReturnGTException pos fname) = "Non-void function " ++ fname ++ " does not return at " ++ showpos pos
    show (WrongReturnTypeGTException pos expected got) = "Wrong return type at " ++ showpos pos ++ ", expected " ++ expected ++ ", got " ++ got

showpos :: BNFC'Position -> String
showpos (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c
showpos Nothing = "unknown position"
