{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Parser.Par
  ( happyError
  , myLexer
  , pTranslationUnit
  ) where

import Prelude

import qualified Parser.Abs
import Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 ((Parser.Abs.BNFC'Position, Parser.Abs.Ident))
happyIn4 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Ident)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ((Parser.Abs.BNFC'Position, Char))
happyIn5 :: ((Parser.Abs.BNFC'Position, Char)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ((Parser.Abs.BNFC'Position, Integer))
happyIn6 :: ((Parser.Abs.BNFC'Position, Integer)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ((Parser.Abs.BNFC'Position, String))
happyIn7 :: ((Parser.Abs.BNFC'Position, String)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 ((Parser.Abs.BNFC'Position, Parser.Abs.TranslationUnit))
happyIn8 :: ((Parser.Abs.BNFC'Position, Parser.Abs.TranslationUnit)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ((Parser.Abs.BNFC'Position, Parser.Abs.Type))
happyIn9 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Type)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ((Parser.Abs.BNFC'Position, [Parser.Abs.Type]))
happyIn10 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.Type])) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ((Parser.Abs.BNFC'Position, Parser.Abs.Arg))
happyIn11 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Arg)) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 ((Parser.Abs.BNFC'Position, [Parser.Abs.Arg]))
happyIn12 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.Arg])) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ((Parser.Abs.BNFC'Position, Parser.Abs.Block))
happyIn13 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Block)) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ((Parser.Abs.BNFC'Position, Parser.Abs.Decl))
happyIn14 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Decl)) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ((Parser.Abs.BNFC'Position, [Parser.Abs.Decl]))
happyIn15 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.Decl])) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ((Parser.Abs.BNFC'Position, Parser.Abs.DItem))
happyIn16 :: ((Parser.Abs.BNFC'Position, Parser.Abs.DItem)) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ((Parser.Abs.BNFC'Position, [Parser.Abs.DItem]))
happyIn17 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.DItem])) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ((Parser.Abs.BNFC'Position, Parser.Abs.DItemConst))
happyIn18 :: ((Parser.Abs.BNFC'Position, Parser.Abs.DItemConst)) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 ((Parser.Abs.BNFC'Position, [Parser.Abs.DItemConst]))
happyIn19 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.DItemConst])) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ((Parser.Abs.BNFC'Position, Parser.Abs.Instr))
happyIn20 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Instr)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ((Parser.Abs.BNFC'Position, [Parser.Abs.Instr]))
happyIn21 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.Instr])) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn22 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn23 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn24 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn25 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn26 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn27 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn28 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn29 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn30 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 ((Parser.Abs.BNFC'Position, Parser.Abs.Expr))
happyIn31 :: ((Parser.Abs.BNFC'Position, Parser.Abs.Expr)) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 ((Parser.Abs.BNFC'Position, [Parser.Abs.Expr]))
happyIn32 :: ((Parser.Abs.BNFC'Position, [Parser.Abs.Expr])) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ((Parser.Abs.BNFC'Position, Parser.Abs.UnaryOp))
happyIn33 :: ((Parser.Abs.BNFC'Position, Parser.Abs.UnaryOp)) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ((Parser.Abs.BNFC'Position, Parser.Abs.MulOp))
happyIn34 :: ((Parser.Abs.BNFC'Position, Parser.Abs.MulOp)) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 ((Parser.Abs.BNFC'Position, Parser.Abs.AddOp))
happyIn35 :: ((Parser.Abs.BNFC'Position, Parser.Abs.AddOp)) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ((Parser.Abs.BNFC'Position, Parser.Abs.RelOp))
happyIn36 :: ((Parser.Abs.BNFC'Position, Parser.Abs.RelOp)) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ((Parser.Abs.BNFC'Position, Parser.Abs.EqOp))
happyIn37 :: ((Parser.Abs.BNFC'Position, Parser.Abs.EqOp)) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ((Parser.Abs.BNFC'Position, Parser.Abs.AssignOp))
happyIn38 :: ((Parser.Abs.BNFC'Position, Parser.Abs.AssignOp)) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x41\xcc\x40\x40\x40\x10\x78\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x40\x00\x00\x20\x0d\x44\x00\x00\x00\x00\x00\x40\x10\x33\x10\x10\x10\x04\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x24\x91\x10\x02\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x41\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x10\x10\x04\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\xd2\x40\x04\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\xd2\x40\x04\x00\x00\x00\x00\x00\x00\x01\x00\x80\x34\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\xd2\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x01\x00\x80\x34\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x00\x10\x04\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x31\x03\x01\x01\x41\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x10\x10\x04\x1e\x00\x00\x00\x00\x10\xc4\x0c\x04\x00\x04\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xc4\x0c\x04\x00\x04\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x00\x10\x04\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xc4\x0c\x04\x00\x04\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x00\x10\x04\x1e\x00\x00\x00\x00\x10\xc4\x0c\x04\x00\x04\x81\x07\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x41\xe0\x01\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\xd2\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\xd2\x40\x04\x00\x00\x00\x00\x00\x00\x01\x00\x80\x34\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x80\x34\x10\x01\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x31\x03\x01\x01\x41\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\xd2\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x08\x00\x00\x00\x00\x00\x10\xc4\x0c\x04\x24\xb5\x99\x07\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xc4\x0c\x04\x24\xb5\x99\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x10\x10\x04\x1e\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x31\x03\x01\x01\x41\xe0\x01\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x10\x10\x04\x1e\x00\x00\x00\x00\x10\xc4\x0c\x04\x04\x04\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x33\x10\x90\xd4\x66\x1e\x00\x00\x00\x00\x10\xc4\x0c\x04\x24\xb5\x99\x07\x00\x00\x00\x00\x04\x31\x03\x01\x01\x41\xe0\x01\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x31\x03\x01\x49\x6d\xe6\x01\x00\x00\x00\x00\x41\xcc\x40\x40\x40\x10\x78\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x31\x03\x01\x49\x6d\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pTranslationUnit_internal","Ident","Char","Integer","String","TranslationUnit","Type","ListType","Arg","ListArg","Block","Decl","ListDecl","DItem","ListDItem","DItemConst","ListDItemConst","Instr","ListInstr","Expr","Expr1","Expr2","Expr3","Expr4","Expr5","Expr6","Expr7","Expr8","Expr9","ListExpr1","UnaryOp","MulOp","AddOp","RelOp","EqOp","AssignOp","'!'","'!='","'%'","'%='","'&'","'&&'","'('","')'","'*'","'*='","'+'","'++'","'+='","','","'-'","'--'","'-='","'->'","'/'","'/='","':'","';'","'<'","'<='","'='","'=='","'=>'","'>'","'>='","'['","'\\\\'","']'","'bool'","'break'","'char'","'const'","'continue'","'else'","'false'","'fn'","'for'","'if'","'int'","'return'","'true'","'var'","'void'","'while'","'{'","'||'","'}'","L_Ident","L_charac","L_integ","L_quoted","%eof"]
        bit_start = st Prelude.* 94
        bit_end = (st Prelude.+ 1) Prelude.* 94
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..93]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xeb\xff\xd3\xff\x00\x00\xd2\xff\xeb\xff\x00\x00\xde\xff\xde\xff\xde\xff\x10\x00\x16\x00\x11\x00\x25\x00\x23\x00\x35\x00\x30\x00\x00\x00\x00\x00\x17\x00\x63\x00\xfc\xff\x00\x00\x17\x00\x71\x00\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x4f\x00\x07\x00\x27\x01\x4b\x00\x37\x00\x00\x00\x79\x02\x18\x00\x00\x00\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x71\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x71\x00\x71\x00\x00\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x48\x00\x53\x00\x5d\x00\x40\x00\x00\x00\x00\x00\x56\x00\x65\x00\xfc\xff\x71\x00\x00\x00\x59\x00\x6e\x00\x86\x00\xfc\xff\x75\x00\x8b\x00\x00\x00\x04\x00\x74\x00\x00\x00\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x00\x74\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x74\x00\x74\x00\x90\x00\x04\x00\x07\x00\x27\x01\x4b\x00\x37\x00\x00\x00\x7f\x00\x94\x00\x9f\x00\x7b\x00\x00\x00\x18\x00\xaa\x00\xad\x00\x71\x00\x00\x00\x00\x00\x00\x00\x71\x00\x71\x00\x00\x00\xac\x00\x00\x00\x71\x00\xbe\x00\x04\x00\x00\x00\x63\x00\x00\x00\x00\x00\x71\x00\x00\x00\x00\x00\xeb\xff\x01\x00\xc8\x00\xbd\x00\x00\x00\x01\x00\xd9\x00\xda\x00\xf8\x00\x03\x01\x18\x01\x19\x01\x63\x00\x1a\x01\x63\x00\x10\x01\x63\x00\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x01\x20\x01\x00\x00\x21\x01\x32\x00\x32\x00\x63\x00\x14\x01\x06\x01\x00\x00\x32\x00\x63\x00\x2f\x01\x00\x00\x32\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xf7\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x0e\x00\x3d\x01\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\xc1\x01\x76\x00\x00\x00\x60\x00\x3b\x01\xdf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x01\x35\x01\x22\x01\x43\x01\x00\x00\x46\x01\x03\x00\x00\x00\x45\x01\x00\x00\x00\x00\x00\x00\x00\x00\x62\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x64\x01\x00\x00\x00\x00\x65\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x85\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x02\x00\x00\x63\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\x01\x08\x01\x00\x00\x00\x00\x00\x00\xea\x00\x00\x00\x00\x00\xcc\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x61\x02\x59\x02\x00\x00\x00\x00\x4c\x01\x52\x01\x54\x01\x56\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x70\x01\x8b\x01\x00\x00\x88\x01\x00\x00\x8d\x01\x00\x00\x00\x00\x00\x00\x6e\x01\x00\x00\x00\x00\x8e\x01\x00\x00\x00\x00\x7c\x00\xa3\x00\x00\x00\x93\x01\x00\x00\xc1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x01\x00\x00\x08\x02\x00\x00\x13\x02\x98\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\x00\xfd\x00\x31\x02\x00\x00\x00\x00\x00\x00\x1b\x01\xb6\x01\x00\x00\x00\x00\x39\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xe6\xff\x00\x00\xfe\xff\x00\x00\xe6\xff\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\xe5\xff\xe8\xff\x00\x00\x00\x00\xed\xff\xe9\xff\x00\x00\x00\x00\x00\x00\xb8\xff\xbc\xff\xbd\xff\xbb\xff\xe3\xff\xd0\xff\xcd\xff\xcb\xff\xc9\xff\xc7\xff\xc5\xff\xc3\xff\xc1\xff\x00\x00\xb0\xff\xd1\xff\xb2\xff\xaf\xff\xb1\xff\xae\xff\x00\x00\x00\x00\xb9\xff\xba\xff\xfd\xff\xfc\xff\xfb\xff\xe4\xff\xf2\xff\x00\x00\xf8\xff\xf7\xff\x00\x00\xf9\xff\xf6\xff\xe1\xff\x00\x00\xec\xff\x00\x00\x00\x00\xe0\xff\xde\xff\x00\x00\x00\x00\xed\xff\x00\x00\xf5\xff\x00\x00\xf1\xff\x00\x00\xed\xff\x00\x00\x00\x00\xd2\xff\xbf\xff\x00\x00\x9f\xff\xb5\xff\xa1\xff\x9e\xff\x9d\xff\xa0\xff\xa2\xff\x00\x00\x00\x00\xab\xff\xad\xff\xac\xff\x00\x00\xaa\xff\xa9\xff\x00\x00\xa8\xff\xa7\xff\xa6\xff\xa5\xff\x00\x00\xa3\xff\xa4\xff\x00\x00\x00\x00\xcc\xff\xc1\xff\xca\xff\xc8\xff\xc6\xff\xc4\xff\xc2\xff\x00\x00\xb4\xff\x00\x00\xcf\xff\xbe\xff\x00\x00\x00\x00\x00\x00\xf2\xff\xf3\xff\xef\xff\xeb\xff\x00\x00\x00\x00\xee\xff\x00\x00\xf0\xff\x00\x00\x00\x00\xc0\xff\xb6\xff\xb5\xff\xb7\xff\xb3\xff\x00\x00\xf4\xff\xe7\xff\xe6\xff\xd1\xff\x00\x00\x00\x00\xdd\xff\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xd7\xff\xd6\xff\xdc\xff\xea\xff\xd3\xff\xce\xff\x00\x00\x00\x00\xd5\xff\x00\x00\xd1\xff\xd1\xff\x00\x00\x00\x00\xdb\xff\xd9\xff\xd1\xff\xd1\xff\x00\x00\xda\xff\xd1\xff\xd8\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x01\x00\x00\x00\x01\x00\x02\x00\x03\x00\x34\x00\x07\x00\x02\x00\x38\x00\x07\x00\x0b\x00\x0c\x00\x00\x00\x24\x00\x0f\x00\x10\x00\x34\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x17\x00\x2e\x00\x05\x00\x06\x00\x0e\x00\x0f\x00\x1b\x00\x07\x00\x1f\x00\x1a\x00\x1e\x00\x22\x00\x0e\x00\x15\x00\x25\x00\x16\x00\x27\x00\x19\x00\x29\x00\x2a\x00\x07\x00\x2c\x00\x2d\x00\x1b\x00\x34\x00\x30\x00\x31\x00\x01\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x07\x00\x03\x00\x00\x00\x19\x00\x0b\x00\x0c\x00\x27\x00\x09\x00\x0f\x00\x10\x00\x0e\x00\x00\x00\x2d\x00\x16\x00\x0c\x00\x0d\x00\x17\x00\x13\x00\x34\x00\x34\x00\x35\x00\x36\x00\x37\x00\x32\x00\x1f\x00\x0e\x00\x0f\x00\x22\x00\x06\x00\x0b\x00\x25\x00\x07\x00\x27\x00\x0f\x00\x29\x00\x2a\x00\x15\x00\x2c\x00\x2d\x00\x00\x00\x0e\x00\x30\x00\x31\x00\x01\x00\x08\x00\x34\x00\x35\x00\x36\x00\x37\x00\x07\x00\x15\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0f\x00\x10\x00\x34\x00\x01\x00\x00\x00\x12\x00\x07\x00\x20\x00\x17\x00\x07\x00\x0e\x00\x07\x00\x08\x00\x0b\x00\x0c\x00\x00\x00\x1f\x00\x0f\x00\x10\x00\x00\x00\x0a\x00\x0b\x00\x07\x00\x08\x00\x27\x00\x17\x00\x07\x00\x08\x00\x08\x00\x1e\x00\x2d\x00\x1c\x00\x21\x00\x08\x00\x23\x00\x24\x00\x06\x00\x34\x00\x35\x00\x36\x00\x37\x00\x27\x00\x2b\x00\x05\x00\x06\x00\x20\x00\x2f\x00\x2d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x34\x00\x35\x00\x36\x00\x37\x00\x09\x00\x32\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x12\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x09\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x31\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x09\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x31\x00\x10\x00\x16\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x04\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0a\x00\x0b\x00\x19\x00\x1a\x00\x1b\x00\x09\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x33\x00\x10\x00\x16\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x16\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x07\x00\x07\x00\x1a\x00\x1b\x00\x09\x00\x1d\x00\x16\x00\x16\x00\x08\x00\x08\x00\x16\x00\x10\x00\x26\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x08\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x17\x00\x18\x00\x05\x00\x1f\x00\x09\x00\x1c\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x21\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x20\x00\x1d\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1e\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x22\x00\x05\x00\x05\x00\x00\x00\x05\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x20\x00\x1f\x00\x1e\x00\x05\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x09\x00\x05\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x09\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x04\x00\x1d\x00\xff\xff\x07\x00\xff\xff\xff\xff\x0a\x00\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\xff\xff\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x41\x00\x28\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x03\x00\x29\x00\x67\x00\xff\xff\x53\x00\x2a\x00\x2b\x00\x0d\x00\x07\x00\x2c\x00\x2d\x00\x03\x00\x08\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x2e\x00\x09\x00\x49\x00\x4a\x00\x0e\x00\x0f\x00\x4f\x00\x29\x00\x2f\x00\x68\x00\x59\x00\x95\x00\x17\x00\x18\x00\x96\x00\x16\x00\x30\x00\x19\x00\x97\x00\x98\x00\x15\x00\x99\x00\x31\x00\x84\x00\x03\x00\x9a\x00\x8d\x00\x28\x00\xd4\xff\x03\x00\x32\x00\x33\x00\x34\x00\x29\x00\x5b\x00\x09\x00\x14\x00\x2a\x00\x2b\x00\x30\x00\x5c\x00\x2c\x00\x2d\x00\x13\x00\x0d\x00\x31\x00\x12\x00\x0a\x00\x0b\x00\x2e\x00\x5d\x00\x03\x00\x03\x00\x32\x00\x33\x00\x34\x00\x6a\x00\x2f\x00\x0e\x00\x42\x00\x95\x00\x69\x00\x5f\x00\x96\x00\x4c\x00\x30\x00\x60\x00\x97\x00\x98\x00\x47\x00\x99\x00\x31\x00\x09\x00\x46\x00\x9a\x00\x8d\x00\x28\x00\x45\x00\x03\x00\x32\x00\x33\x00\x34\x00\x29\x00\x7f\x00\x0a\x00\x3c\x00\x2a\x00\x2b\x00\x04\x00\x10\x00\x2c\x00\x2d\x00\x03\x00\x28\x00\x3d\x00\x7e\x00\x36\x00\x7b\x00\x2e\x00\x29\x00\x7a\x00\x3e\x00\x3f\x00\x2a\x00\x2b\x00\x3d\x00\x2f\x00\x2c\x00\x2d\x00\x3d\x00\x04\x00\x8d\x00\x3e\x00\x7c\x00\x30\x00\x2e\x00\x3e\x00\x77\x00\x79\x00\x37\x00\x31\x00\x77\x00\x38\x00\x76\x00\x39\x00\x3a\x00\x69\x00\x03\x00\x32\x00\x33\x00\x34\x00\x30\x00\x3b\x00\x49\x00\x81\x00\x88\x00\x3c\x00\x31\x00\x87\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x86\x00\x03\x00\x32\x00\x33\x00\x34\x00\x90\x00\x6a\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x84\x00\x91\x00\x92\x00\x93\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x83\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x6d\x00\x22\x00\x23\x00\x24\x00\x6b\x00\x90\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x8a\x00\x91\x00\xa2\x00\x93\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x8d\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x90\x00\x6e\x00\x23\x00\x24\x00\x6b\x00\x90\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x8d\x00\xad\x00\xa1\x00\x93\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x03\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x04\x00\x05\x00\x6f\x00\x24\x00\x6b\x00\x90\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\xa2\x00\xac\x00\xa0\x00\x93\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x9f\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x9e\x00\x9d\x00\x9b\x00\x70\x00\x6b\x00\x90\x00\x26\x00\xa7\x00\xab\x00\xaa\x00\xa9\x00\xb0\x00\xb1\x00\xaf\x00\x93\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\xb3\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x0c\x00\x62\x00\x63\x00\x34\x00\x5d\x00\x90\x00\x64\x00\x65\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\xb3\x00\x65\x00\x93\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x60\x00\x26\x00\x4d\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x59\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x4c\x00\x50\x00\x48\x00\x47\x00\x43\x00\x7b\x00\x65\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x60\x00\x5d\x00\x59\x00\x80\x00\x72\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x73\x00\x26\x00\x72\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x88\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x7f\x00\x8b\x00\x8a\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\xa3\x00\x00\x00\x9b\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\xa4\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x4e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x41\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x71\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa7\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\xa5\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x26\x00\x74\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x6b\x00\x00\x00\x26\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x6b\x00\x00\x00\x26\x00\x6c\x00\x21\x00\x22\x00\x23\x00\x24\x00\x6b\x00\x52\x00\x26\x00\x00\x00\x53\x00\x00\x00\x00\x00\x54\x00\x00\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 98) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98)
	]

happy_n_terms = 57 :: Prelude.Int
happy_n_nonterms = 35 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.Ident (tokenText happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Char)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Integer)
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), ((\(PT _ (TL s)) -> s) happy_var_1))
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn8
		 ((fst happy_var_1, Parser.Abs.Program (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TInt (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_7 = happySpecReduce_1  5# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TBool (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_8 = happySpecReduce_1  5# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TChar (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_9 = happySpecReduce_1  5# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TVoid (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_10 = happySpecReduce_2  5# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TConst (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_11 = happyReduce 5# 5# happyReduction_11
happyReduction_11 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut9 happy_x_5 of { (HappyWrap9 happy_var_5) -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TFunc (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_12 = happySpecReduce_3  5# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn9
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.TArray (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_13 = happySpecReduce_0  6# happyReduction_13
happyReduction_13  =  happyIn10
		 ((Parser.Abs.BNFC'NoPosition, [])
	)

happyReduce_14 = happySpecReduce_1  6# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn10
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_15 = happySpecReduce_3  6# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_16 = happySpecReduce_3  7# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn11
		 ((fst happy_var_1, Parser.Abs.PArgVal (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_17 = happyReduce 4# 7# happyReduction_17
happyReduction_17 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	happyIn11
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.PArgRef (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_18 = happySpecReduce_0  8# happyReduction_18
happyReduction_18  =  happyIn12
		 ((Parser.Abs.BNFC'NoPosition, [])
	)

happyReduce_19 = happySpecReduce_1  8# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn12
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_20 = happySpecReduce_3  8# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_21 = happyReduce 4# 9# happyReduction_21
happyReduction_21 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn13
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.PBlock (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_22 = happySpecReduce_3  10# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn14
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.DVar (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_23 = happySpecReduce_3  10# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { (HappyWrap19 happy_var_2) -> 
	happyIn14
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.DConst (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_24 = happyReduce 8# 10# happyReduction_24
happyReduction_24 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	case happyOut9 happy_x_7 of { (HappyWrap9 happy_var_7) -> 
	case happyOut13 happy_x_8 of { (HappyWrap13 happy_var_8) -> 
	happyIn14
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.DFunc (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_7) (snd happy_var_8))
	) `HappyStk` happyRest}}}}}

happyReduce_25 = happySpecReduce_0  11# happyReduction_25
happyReduction_25  =  happyIn15
		 ((Parser.Abs.BNFC'NoPosition, [])
	)

happyReduce_26 = happySpecReduce_2  11# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn15
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_27 = happySpecReduce_3  12# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn16
		 ((fst happy_var_1, Parser.Abs.DItemNoInit (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_28 = happySpecReduce_3  12# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	happyIn16
		 ((fst happy_var_1, Parser.Abs.DItemInit (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_29 = happySpecReduce_1  13# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn17
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_30 = happySpecReduce_3  13# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn17
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_31 = happySpecReduce_3  14# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	happyIn18
		 ((fst happy_var_1, Parser.Abs.DItemConstInit (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_32 = happySpecReduce_1  15# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn19
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_33 = happySpecReduce_3  15# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn19
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_34 = happySpecReduce_1  16# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, Parser.Abs.IBlock (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_35 = happySpecReduce_2  16# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, Parser.Abs.IExpr (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_36 = happyReduce 5# 16# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	case happyOut20 happy_x_5 of { (HappyWrap20 happy_var_5) -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IIf (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_37 = happyReduce 7# 16# happyReduction_37
happyReduction_37 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	case happyOut20 happy_x_5 of { (HappyWrap20 happy_var_5) -> 
	case happyOut20 happy_x_7 of { (HappyWrap20 happy_var_7) -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IIfElse (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_38 = happyReduce 5# 16# happyReduction_38
happyReduction_38 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	case happyOut20 happy_x_5 of { (HappyWrap20 happy_var_5) -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IWhile (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_39 = happyReduce 9# 16# happyReduction_39
happyReduction_39 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	case happyOut23 happy_x_5 of { (HappyWrap23 happy_var_5) -> 
	case happyOut22 happy_x_7 of { (HappyWrap22 happy_var_7) -> 
	case happyOut20 happy_x_9 of { (HappyWrap20 happy_var_9) -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IFor (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7) (snd happy_var_9))
	) `HappyStk` happyRest}}}}}

happyReduce_40 = happySpecReduce_2  16# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IContinue (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_41 = happySpecReduce_2  16# happyReduction_41
happyReduction_41 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IBreak (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_42 = happySpecReduce_3  16# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	happyIn20
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.IReturn (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_43 = happySpecReduce_0  17# happyReduction_43
happyReduction_43  =  happyIn21
		 ((Parser.Abs.BNFC'NoPosition, [])
	)

happyReduce_44 = happySpecReduce_2  17# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut21 happy_x_2 of { (HappyWrap21 happy_var_2) -> 
	happyIn21
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_45 = happySpecReduce_1  18# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn22
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_46 = happySpecReduce_0  18# happyReduction_46
happyReduction_46  =  happyIn22
		 ((Parser.Abs.BNFC'NoPosition, Parser.Abs.EEmpty Parser.Abs.BNFC'NoPosition)
	)

happyReduce_47 = happySpecReduce_1  19# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn23
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_48 = happySpecReduce_3  19# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	happyIn23
		 ((fst happy_var_1, Parser.Abs.EAssign (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_49 = happyReduce 8# 19# happyReduction_49
happyReduction_49 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut9 happy_x_6 of { (HappyWrap9 happy_var_6) -> 
	case happyOut13 happy_x_8 of { (HappyWrap13 happy_var_8) -> 
	happyIn23
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.ELambda (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_6) (snd happy_var_8))
	) `HappyStk` happyRest}}}}

happyReduce_50 = happySpecReduce_1  20# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn24
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_51 = happySpecReduce_3  20# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn24
		 ((fst happy_var_1, Parser.Abs.EOr (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_52 = happySpecReduce_1  21# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn25
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_53 = happySpecReduce_3  21# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn25
		 ((fst happy_var_1, Parser.Abs.EAnd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_54 = happySpecReduce_1  22# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn26
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_55 = happySpecReduce_3  22# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn26
		 ((fst happy_var_1, Parser.Abs.EEq (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_56 = happySpecReduce_1  23# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn27
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_57 = happySpecReduce_3  23# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 ((fst happy_var_1, Parser.Abs.ERel (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_58 = happySpecReduce_1  24# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn28
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_59 = happySpecReduce_3  24# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 ((fst happy_var_1, Parser.Abs.EAdd (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_60 = happySpecReduce_1  25# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn29
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_61 = happySpecReduce_3  25# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn29
		 ((fst happy_var_1, Parser.Abs.EMul (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_62 = happySpecReduce_1  26# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn30
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_63 = happyReduce 4# 26# happyReduction_63
happyReduction_63 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut31 happy_x_4 of { (HappyWrap31 happy_var_4) -> 
	happyIn30
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.ECast (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_64 = happySpecReduce_2  26# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	happyIn30
		 ((fst happy_var_1, Parser.Abs.EUOp (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_65 = happySpecReduce_3  27# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	happyIn31
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
	)}}

happyReduce_66 = happySpecReduce_1  27# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn31
		 ((fst happy_var_1, Parser.Abs.ELitInt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_67 = happySpecReduce_1  27# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn31
		 ((fst happy_var_1, Parser.Abs.ELitChar (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_68 = happySpecReduce_1  27# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn31
		 ((fst happy_var_1, Parser.Abs.ELitString (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_69 = happySpecReduce_1  27# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.ELitTrue (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_70 = happySpecReduce_1  27# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.ELitFalse (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_71 = happySpecReduce_1  27# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn31
		 ((fst happy_var_1, Parser.Abs.EIdent (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_72 = happyReduce 4# 27# happyReduction_72
happyReduction_72 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	happyIn31
		 ((fst happy_var_1, Parser.Abs.EIndex (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_73 = happyReduce 4# 27# happyReduction_73
happyReduction_73 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut32 happy_x_3 of { (HappyWrap32 happy_var_3) -> 
	happyIn31
		 ((fst happy_var_1, Parser.Abs.EApply (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_74 = happySpecReduce_0  28# happyReduction_74
happyReduction_74  =  happyIn32
		 ((Parser.Abs.BNFC'NoPosition, [])
	)

happyReduce_75 = happySpecReduce_1  28# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn32
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_76 = happySpecReduce_3  28# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut32 happy_x_3 of { (HappyWrap32 happy_var_3) -> 
	happyIn32
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_77 = happySpecReduce_1  29# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpUnaryPlus (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_78 = happySpecReduce_1  29# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpUnaryMinus (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_79 = happySpecReduce_1  29# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpUnaryBang (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_80 = happySpecReduce_1  29# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpUnaryInc (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_81 = happySpecReduce_1  29# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpUnaryDec (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_82 = happySpecReduce_1  30# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpTimes (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_83 = happySpecReduce_1  30# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpDiv (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_84 = happySpecReduce_1  30# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpMod (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_85 = happySpecReduce_1  31# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpPlus (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_86 = happySpecReduce_1  31# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn35
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpMinus (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_87 = happySpecReduce_1  32# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpLT (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_88 = happySpecReduce_1  32# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpLE (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_89 = happySpecReduce_1  32# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpGT (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_90 = happySpecReduce_1  32# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpGE (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_91 = happySpecReduce_1  33# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpEq (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_92 = happySpecReduce_1  33# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpNeq (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_93 = happySpecReduce_1  34# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpAssign (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_94 = happySpecReduce_1  34# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpAssignTimes (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_95 = happySpecReduce_1  34# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpAssignDiv (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_96 = happySpecReduce_1  34# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpAssignMod (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_97 = happySpecReduce_1  34# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpAssignPlus (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_98 = happySpecReduce_1  34# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1), Parser.Abs.OpAssignMinus (uncurry Parser.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyNewToken action sts stk [] =
	happyDoAction 56# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TV _) -> cont 52#;
	PT _ (TC _) -> cont 53#;
	PT _ (TI _) -> cont 54#;
	PT _ (TL _) -> cont 55#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 56# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pTranslationUnit_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap8 x') = happyOut8 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pTranslationUnit :: [Token] -> Err Parser.Abs.TranslationUnit
pTranslationUnit = fmap snd . pTranslationUnit_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
