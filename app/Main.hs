module Main where

import qualified Interpreter (interpret)

main :: IO ()
main = do
  Interpreter.interpret
