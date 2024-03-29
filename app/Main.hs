module Main where

import System.Environment
import System.Exit

import qualified Interpreter

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> usage
        [f] -> Interpreter.interpretFile f
        [] -> getContents >>= Interpreter.interpret
        _ | otherwise -> usage

usage :: IO ()
usage = do
    putStrLn "gt <file>"
    exitFailure
