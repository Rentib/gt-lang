module Typechecker.Typechecker where

import Common.Exceptions
import Parser.Abs

typecheck :: TranslationUnit -> Either GTException ()
typecheck _ = Right () -- TODO: implement 
