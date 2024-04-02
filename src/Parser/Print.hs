-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Parser.

module Parser.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Parser.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Parser.Abs.Ident where
  prt _ (Parser.Abs.Ident i) = doc $ showString i
instance Print (Parser.Abs.TranslationUnit' a) where
  prt i = \case
    Parser.Abs.Program _ decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print (Parser.Abs.Type' a) where
  prt i = \case
    Parser.Abs.TInt _ -> prPrec i 0 (concatD [doc (showString "int")])
    Parser.Abs.TBool _ -> prPrec i 0 (concatD [doc (showString "bool")])
    Parser.Abs.TChar _ -> prPrec i 0 (concatD [doc (showString "char")])
    Parser.Abs.TVoid _ -> prPrec i 0 (concatD [doc (showString "void")])
    Parser.Abs.TConst _ type_ -> prPrec i 0 (concatD [doc (showString "const"), prt 0 type_])
    Parser.Abs.TFunc _ types type_ -> prPrec i 0 (concatD [doc (showString "("), prt 0 types, doc (showString ")"), doc (showString "->"), prt 0 type_])
    Parser.Abs.TArray _ type_ -> prPrec i 0 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])

instance Print [Parser.Abs.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Parser.Abs.Arg' a) where
  prt i = \case
    Parser.Abs.PArgVal _ id_ type_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":"), prt 0 type_])
    Parser.Abs.PArgRef _ id_ type_ -> prPrec i 0 (concatD [doc (showString "&"), prt 0 id_, doc (showString ":"), prt 0 type_])

instance Print [Parser.Abs.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Parser.Abs.Block' a) where
  prt i = \case
    Parser.Abs.PBlock _ decls instrs -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decls, prt 0 instrs, doc (showString "}")])

instance Print (Parser.Abs.Decl' a) where
  prt i = \case
    Parser.Abs.DVar _ ditems -> prPrec i 0 (concatD [doc (showString "var"), prt 0 ditems, doc (showString ";")])
    Parser.Abs.DConst _ ditemconsts -> prPrec i 0 (concatD [doc (showString "const"), prt 0 ditemconsts, doc (showString ";")])
    Parser.Abs.DFunc _ id_ args type_ block -> prPrec i 0 (concatD [doc (showString "fn"), prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString "->"), prt 0 type_, prt 0 block])

instance Print [Parser.Abs.Decl' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Parser.Abs.DItem' a) where
  prt i = \case
    Parser.Abs.DItemNoInit _ id_ type_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":"), prt 0 type_])
    Parser.Abs.DItemInit _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [Parser.Abs.DItem' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Parser.Abs.DItemConst' a) where
  prt i = \case
    Parser.Abs.DItemConstInit _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [Parser.Abs.DItemConst' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Parser.Abs.Instr' a) where
  prt i = \case
    Parser.Abs.IBlock _ block -> prPrec i 0 (concatD [prt 0 block])
    Parser.Abs.IExpr _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
    Parser.Abs.IIf _ expr instr -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 instr])
    Parser.Abs.IIfElse _ expr instr1 instr2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 instr1, doc (showString "else"), prt 0 instr2])
    Parser.Abs.IWhile _ expr instr -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 instr])
    Parser.Abs.IFor _ expr1 expr2 expr3 instr -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 expr1, doc (showString ";"), prt 0 expr2, doc (showString ";"), prt 0 expr3, doc (showString ")"), prt 0 instr])
    Parser.Abs.IContinue _ -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    Parser.Abs.IBreak _ -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    Parser.Abs.IReturn _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])

instance Print [Parser.Abs.Instr' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Parser.Abs.Expr' a) where
  prt i = \case
    Parser.Abs.ELitInt _ n -> prPrec i 9 (concatD [prt 0 n])
    Parser.Abs.ELitChar _ c -> prPrec i 9 (concatD [prt 0 c])
    Parser.Abs.ELitString _ str -> prPrec i 9 (concatD [printString str])
    Parser.Abs.ELitTrue _ -> prPrec i 9 (concatD [doc (showString "true")])
    Parser.Abs.ELitFalse _ -> prPrec i 9 (concatD [doc (showString "false")])
    Parser.Abs.EIdent _ id_ -> prPrec i 9 (concatD [prt 0 id_])
    Parser.Abs.EIndex _ expr1 expr2 -> prPrec i 9 (concatD [prt 9 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    Parser.Abs.EApply _ expr exprs -> prPrec i 9 (concatD [prt 9 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Parser.Abs.ECast _ type_ expr -> prPrec i 8 (concatD [doc (showString "("), prt 0 type_, doc (showString ")"), prt 9 expr])
    Parser.Abs.EUOp _ unaryop expr -> prPrec i 8 (concatD [prt 0 unaryop, prt 9 expr])
    Parser.Abs.EMul _ expr1 mulop expr2 -> prPrec i 7 (concatD [prt 7 expr1, prt 0 mulop, prt 8 expr2])
    Parser.Abs.EAdd _ expr1 addop expr2 -> prPrec i 6 (concatD [prt 6 expr1, prt 0 addop, prt 7 expr2])
    Parser.Abs.ERel _ expr1 relop expr2 -> prPrec i 5 (concatD [prt 5 expr1, prt 0 relop, prt 6 expr2])
    Parser.Abs.EEq _ expr1 eqop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 eqop, prt 5 expr2])
    Parser.Abs.EAnd _ expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "&&"), prt 4 expr2])
    Parser.Abs.EOr _ expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "||"), prt 3 expr2])
    Parser.Abs.EAssign _ expr1 assignop expr2 -> prPrec i 1 (concatD [prt 9 expr1, prt 0 assignop, prt 2 expr2])
    Parser.Abs.ELambda _ args type_ block -> prPrec i 0 (concatD [doc (showString "\\"), doc (showString "("), prt 0 args, doc (showString ")"), doc (showString "->"), prt 0 type_, doc (showString "=>"), prt 0 block])
    Parser.Abs.EEmpty _ -> prPrec i 0 (concatD [])

instance Print [Parser.Abs.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Parser.Abs.UnaryOp' a) where
  prt i = \case
    Parser.Abs.OpUnaryPlus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Parser.Abs.OpUnaryMinus _ -> prPrec i 0 (concatD [doc (showString "-")])
    Parser.Abs.OpUnaryBang _ -> prPrec i 0 (concatD [doc (showString "!")])

instance Print (Parser.Abs.MulOp' a) where
  prt i = \case
    Parser.Abs.OpTimes _ -> prPrec i 0 (concatD [doc (showString "*")])
    Parser.Abs.OpDiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    Parser.Abs.OpMod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (Parser.Abs.AddOp' a) where
  prt i = \case
    Parser.Abs.OpPlus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Parser.Abs.OpMinus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (Parser.Abs.RelOp' a) where
  prt i = \case
    Parser.Abs.OpLT _ -> prPrec i 0 (concatD [doc (showString "<")])
    Parser.Abs.OpLE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    Parser.Abs.OpGT _ -> prPrec i 0 (concatD [doc (showString ">")])
    Parser.Abs.OpGE _ -> prPrec i 0 (concatD [doc (showString ">=")])

instance Print (Parser.Abs.EqOp' a) where
  prt i = \case
    Parser.Abs.OpEq _ -> prPrec i 0 (concatD [doc (showString "==")])
    Parser.Abs.OpNeq _ -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print (Parser.Abs.AssignOp' a) where
  prt i = \case
    Parser.Abs.OpAssign _ -> prPrec i 0 (concatD [doc (showString "=")])
    Parser.Abs.OpAssignTimes _ -> prPrec i 0 (concatD [doc (showString "*=")])
    Parser.Abs.OpAssignDiv _ -> prPrec i 0 (concatD [doc (showString "/=")])
    Parser.Abs.OpAssignMod _ -> prPrec i 0 (concatD [doc (showString "%=")])
    Parser.Abs.OpAssignPlus _ -> prPrec i 0 (concatD [doc (showString "+=")])
    Parser.Abs.OpAssignMinus _ -> prPrec i 0 (concatD [doc (showString "-=")])
