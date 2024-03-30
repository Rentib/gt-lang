{-# LANGUAGE FlexibleInstances #-}

module Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.State
import Data.Array

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
    eval (Program pos decls) =
        mapM_ eval decls >> eval (EApply pos (EIdent pos (Ident "main")) [])

instance Evaluator Block where
    eval (PBlock _ decls instrs) = do
        es <- get
        mapM_ eval decls >> mapM_ eval instrs
        es' <- get
        put $ es'{env = env es}
        pure VVoid

instance Evaluator Decl where
    eval (DNoInit _ x _) = modify (esNew x VUninitialized) >> pure VVoid
    eval (DInit _ x e) = eval e >>= \v -> modify (esNew x v) >> pure VVoid
    eval (DConst _ x e) = eval e >>= \v -> modify (esNew x v) >> pure VVoid
    eval (DFunc _ f args _ block) = do
        es <- get
        modify $ esNew f (VFunc args block (env es))
        pure VVoid

evalIfNoFlag :: EvalM -> EvalM
evalIfNoFlag m = do
    es <- get
    case flag es of
        ESFNone -> m
        _ | otherwise -> pure VVoid

instance Evaluator Instr where
    eval (IBlock _ block) = evalIfNoFlag $ eval block
    eval (IExpr _ e) = evalIfNoFlag $ eval e
    eval (IIf _ e i) = evalIfNoFlag $ do
        eval e >>= \v -> if v == VBool True then eval i else pure VVoid
    eval (IIfElse _ e i1 i2) = evalIfNoFlag $ do
        eval e >>= \v -> if v == VBool True then eval i1 else eval i2
    eval (IWhile pos e i) = evalIfNoFlag $ do
        v <- eval e
        if v == VBool True
            then do
                void $ eval i
                es <- get
                case flag es of
                    ESFContinue -> modify (esPutFlag ESFNone) >> eval (IWhile pos e i)
                    ESFBreak -> modify (esPutFlag ESFNone) >> pure VVoid
                    _ | otherwise -> eval (IWhile pos e i)
            else pure VVoid
    eval (IDo pos i e) = evalIfNoFlag $ eval i >> eval (IWhile pos e i)
    eval (IFor pos e1 e2 e3 i) =
        evalIfNoFlag $
            eval e1 >> eval (IWhile pos e2 (IBlock pos (PBlock pos [] [i, IExpr pos e3])))
    eval (IContinue _) = evalIfNoFlag $ modify (esPutFlag ESFContinue) >> pure VVoid
    eval (IBreak _) = evalIfNoFlag $ modify (esPutFlag ESFBreak) >> pure VVoid
    eval (IReturn _ e) = evalIfNoFlag $ eval e >>= \v -> modify (esPutFlag $ ESFReturn v) >> pure VVoid

evalBuiltin :: Expr -> [Expr] -> EvalM -> EvalM
evalBuiltin (EIdent _ (Ident "print")) args _ =
    mapM eval args >>= \vs -> liftIO $ mapM_ (putStr . show) vs >> pure VVoid
evalBuiltin (EIdent pos (Ident "malloc")) [e] _ = do
    v <- eval e
    case v of
        VInt n -> pure $ VArray $ array (0, n - 1) [(i, VUninitialized) | i <- [0 .. n - 1]]
        _ | otherwise -> throwError $ UnknownRuntimeGTException pos
evalBuiltin _ _ em = em

instance Evaluator Expr where
    eval (ELitInt _ n) = pure $ VInt n
    eval (ELitChar _ c) = pure $ VChar c
    eval (ELitString _ s) =
        pure $ VArray $ array (0, fromIntegral (length s - 1)) [(i, VChar c) | (i, c) <- zip [0 ..] s]
    eval (ELitTrue _) = pure $ VBool True
    eval (ELitFalse _) = pure $ VBool False
    eval (EIdent _ x) = gets $ esGet x
    eval (EIndex pos e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VArray a, VInt i) -> pure $ a ! i
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EApply pos e args) = evalBuiltin e args $ do
        f <- eval e
        (VFunc params block fenv) <- case f of
            VFunc{} -> pure f
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
        when (length params /= length args) $ throwError $ UnknownRuntimeGTException pos
        es <- get
        -- gets location of argument (for passing by reference)
        let getLoc (EIdent _ x) = Just $ envGet x (env es)
            getLoc _ = Nothing
        let esPutArg :: (Arg, Value, Maybe Loc) -> EvalM
            esPutArg (PArgRef _ x _, _, Just l) =
                get >>= \es' -> put (es'{env = envPut x l (env es')}) >> pure VVoid
            esPutArg (PArgVal _ x _, v, _) = modify (esNew x v) >> pure VVoid
            esPutArg _ = throwError $ UnknownRuntimeGTException pos

        -- evaluate arguments in current environment
        arg_values <- mapM eval args
        let arg_locs = map getLoc args
        -- change environment and set up params
        put $ es{env = fenv}
        mapM_ esPutArg $ zip3 params arg_values arg_locs

        -- recursion
        void $ case e of
            EIdent _ (Ident fname) -> modify (esNew (Ident fname) f)
            _ | otherwise -> pure ()

        void $ eval block
        res <- gets esGetFlag
        es' <- get
        put $ es{store = store es'} -- TODO: check if it is correct

        -- TODO: if there is no return, check if function should return void
        case res of
            ESFReturn v -> pure v
            ESFNone -> pure VVoid
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EUOp pos op e) = do
        v <- eval e
        case op of
            OpUnaryPlus _ -> case v of
                VInt n -> pure $ VInt n
                _ | otherwise -> throwError $ UnknownRuntimeGTException pos
            OpUnaryMinus _ -> case v of
                VInt n -> pure $ VInt (-n)
                _ | otherwise -> throwError $ UnknownRuntimeGTException pos
            OpUnaryBang _ -> case v of
                VBool b -> pure $ VBool $ not b
                _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EMul pos e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VInt n1, VInt n2) -> case op of
                OpTimes _ -> pure $ VInt $ n1 * n2
                OpDiv pos' -> do
                    when (n2 == 0) $ throwError $ DivideByZeroGTException pos'
                    pure $ VInt $ n1 `div` n2
                OpMod pos' -> do
                    when (n2 == 0) $ throwError $ DivideByZeroGTException pos'
                    pure $ VInt $ n1 `mod` n2
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EAdd pos e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VInt n1, VInt n2) -> pure $ VInt $ case op of
                OpPlus _ -> n1 + n2
                OpMinus _ -> n1 - n2
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (ERel _ e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        pure $ VBool $ case op of
            OpLT _ -> v1 < v2
            OpLE _ -> v1 <= v2
            OpGT _ -> v1 > v2
            OpGE _ -> v1 >= v2
    eval (EEq _ e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        pure $ VBool $ case op of
            OpEq _ -> v1 == v2
            OpNeq _ -> v1 /= v2
    eval (EAnd _ e1 e2) = eval e1 >>= \v1 -> if v1 == VBool True then eval e2 else pure v1
    eval (EOr _ e1 e2) = eval e1 >>= \v1 -> if v1 == VBool True then pure v1 else eval e2
    eval (EAssign pos e1 _ e2) = do
        -- TODO: implement properly
        v2 <- eval e2
        case e1 of
            EIdent _ x -> do
                modify $ esUpdate x v2
                pure v2
            EIndex _ (EIdent _ x) idx -> do
                v1 <- gets $ esGet x
                i <- eval idx
                case (v1, i) of
                    (VArray a, VInt i') -> do
                        let a' = a // [(i', v2)]
                        modify $ esUpdate x (VArray a')
                        pure v2
                    _ | otherwise -> throwError $ UnknownRuntimeGTException pos
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (ELambda _ args _ block) = gets (VFunc args block . env)
    eval (EEmpty _) = pure VVoid
