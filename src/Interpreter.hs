module Interpreter (interpret, interpretFile) where

import System.Exit

import Parser.Abs
import Parser.Par

import Evaluator.Evaluator (evaluate)
import Typechecker.Typechecker (typecheck)

interpretFile :: FilePath -> IO ()
interpretFile f = readFile f >>= interpret

interpret :: String -> IO ()
interpret s = tcrun (pTranslationUnit (myLexer s))

tcrun :: Either String TranslationUnit -> IO ()
tcrun (Left err) = die err
tcrun (Right tree) = do
    case typecheck tree of
        Left err -> die $ show err
        Right _ -> do
            res <- evaluate tree
            case res of
                Left err -> die $ show err
                Right _ -> exitSuccess
