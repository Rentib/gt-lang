{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.State
import Data.Array

import Common.Exceptions
import Common.Utils
import Evaluator.Datatypes
import Parser.Abs

evaluate :: TranslationUnit -> IO (Either GTException Value)
evaluate tu = evalStateT (runExceptT (eval tu)) esEmpty

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
    eval (DVar _ xs) = do
        let addItem :: DItem -> EvalM
            addItem (DItemNoInit _ x _) = modify (esNew x VUninitialized) >> pure VVoid
            addItem (DItemInit _ x e) = eval e >>= \v -> modify (esNew x v) >> pure VVoid
        mapM_ addItem xs >> pure VVoid
    eval (DConst _ xs) = do
        let addItem :: DItemConst -> EvalM
            addItem (DItemConstInit _ x e) = eval e >>= \v -> modify (esNew x v) >> pure VVoid
        mapM_ addItem xs >> pure VVoid
    eval (DFunc _ fname args _ block) = do
        es <- get
        modify $ esNew fname (VFunc fname args block (env es))
        pure VVoid

evalIfNoFlag :: EvalM -> EvalM
evalIfNoFlag m =
    gets esGetFlag >>= \case
        ESFNone -> m
        _ | otherwise -> pure VVoid

instance Evaluator Instr where
    eval (IBlock _ block) = evalIfNoFlag $ eval block
    eval (IExpr _ e) = evalIfNoFlag $ eval e
    eval (IIf _ e i) = evalIfNoFlag $ do
        eval e >>= \v -> if v == VBool True then eval i else pure VVoid
    eval (IIfElse _ e i1 i2) = evalIfNoFlag $ do
        eval e >>= \v -> if v == VBool True then eval i1 else eval i2
    eval (IWhile pos e i) = eval $ IFor pos (EEmpty pos) e (EEmpty pos) i
    eval (IFor _ e1 e2 e3 i) = evalIfNoFlag $ do
        let loop = evalIfNoFlag $ do
                v <- eval e2
                if v == VBool True
                    then do
                        void $ eval i
                        gets esGetFlag >>= \case
                            ESFContinue -> modify (esPutFlag ESFNone) >> eval e3 >> loop
                            ESFBreak -> modify (esPutFlag ESFNone) >> pure VVoid
                            _ | otherwise -> eval e3 >> loop
                    else pure VVoid
        eval e1 >> loop
    eval (IContinue _) = evalIfNoFlag $ modify (esPutFlag ESFContinue) >> pure VVoid
    eval (IBreak _) = evalIfNoFlag $ modify (esPutFlag ESFBreak) >> pure VVoid
    eval (IReturn _ e) = evalIfNoFlag $ eval e >>= \v -> modify (esPutFlag $ ESFReturn v) >> pure VVoid

evalBuiltin :: Expr -> [Expr] -> EvalM -> EvalM
evalBuiltin (EIdent _ (Ident "print")) args _ =
    mapM eval args >>= \vs -> liftIO $ mapM_ (putStr . show) vs >> pure VVoid
evalBuiltin (EIdent pos (Ident "malloc")) [e] _ = do
    eval e >>= \case
        VInt n -> pure $ VArray $ array (0, n - 1) [(i, VUninitialized) | i <- [0 .. n - 1]]
        _ | otherwise -> throwError $ UnknownRuntimeGTException pos
evalBuiltin _ _ em = em

evalBinary :: Expr -> Expr -> ((Value, Value) -> EvalM) -> EvalM
evalBinary e1 e2 f = eval e1 >>= \v1 -> eval e2 >>= \v2 -> f (v1, v2)

checkBounds :: BNFC'Position -> Array Integer a -> Integer -> EvalM
checkBounds pos a i =
    if i < 0 || i > fromIntegral (snd (bounds a))
        then throwError $ IndexOutOfBoundsGTException pos i
        else pure VVoid

evalCast :: Value -> Type -> Value
evalCast VUninitialized _ = VUninitialized
evalCast (VInt n) (TInt _) = VInt n
evalCast (VInt n) (TBool _) = VBool $ n /= 0
evalCast (VInt n) (TChar _) = VChar $ toEnum $ fromIntegral n
evalCast (VInt _) (TVoid _) = VVoid
evalCast (VBool b) (TInt _) = VInt $ if b then 1 else 0
evalCast (VBool b) (TBool _) = VBool b
evalCast (VBool b) (TChar _) = VChar $ if b then '1' else '0'
evalCast (VBool _) (TVoid _) = VVoid
evalCast (VChar c) (TInt _) = VInt $ fromIntegral $ fromEnum c
evalCast (VChar c) (TBool _) = VBool $ c /= '\0'
evalCast (VChar c) (TChar _) = VChar c
evalCast (VChar _) (TVoid _) = VVoid
evalCast VVoid (TInt _) = VInt 0
evalCast VVoid (TBool _) = VBool False
evalCast VVoid (TChar _) = VChar '\0'
evalCast VVoid (TVoid _) = VVoid
evalCast (VArray arr) (TArray _ t) = VArray $ array (bounds arr) [(i, evalCast v t) | (i, v) <- assocs arr]
evalCast _ _ = undefined

instance Evaluator Expr where
    eval (ELitInt _ n) = pure $ VInt n
    eval (ELitChar _ c) = pure $ VChar c
    eval (ELitString _ s) =
        pure $ VArray $ array (0, fromIntegral (length s - 1)) [(i, VChar c) | (i, c) <- zip [0 ..] s]
    eval (ELitTrue _) = pure $ VBool True
    eval (ELitFalse _) = pure $ VBool False
    eval (EIdent _ x) = gets $ esGet x
    eval (EIndex pos e1 e2) = do
        evalBinary e1 e2 $ \case
            (VArray a, VInt i) -> checkBounds pos a i >> pure (a ! i)
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EApply pos e args) = evalBuiltin e args $ do
        f <- eval e
        (VFunc fname params block fenv) <- case f of
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

        -- recursion, FIXME: doesnt work after function assignment
        modify $ esNew fname f
        -- void $ case e of
        --     EIdent _ (Ident fname) -> modify (esNew (Ident fname) f)
        --     _ | otherwise -> pure ()

        void $ eval block
        res <- gets esGetFlag
        es' <- get
        put $ es{store = store es'} -- TODO: check if it is correct

        -- TODO: if there is no return, check if function should return void
        case res of
            ESFReturn v -> pure v
            ESFNone -> pure VVoid
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (ECast _ t e) = eval e >>= \v -> pure $ evalCast v t
    eval (EUOp pos (OpUnaryPlus _) e) =
        eval e >>= \case
            VInt n -> pure $ VInt n
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EUOp pos (OpUnaryMinus _) e) =
        eval e >>= \case
            VInt n -> pure $ VInt $ -n
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EUOp pos (OpUnaryBang _) e) =
        eval e >>= \case
            VBool b -> pure $ VBool $ not b
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EMul pos e1 op e2) =
        evalBinary e1 e2 $ \case
            (VInt n1, VInt n2) -> case op of
                OpTimes _ -> pure $ VInt $ n1 * n2
                OpDiv pos' -> do
                    when (n2 == 0) $ throwError $ DivideByZeroGTException pos'
                    pure $ VInt $ n1 `div` n2
                OpMod pos' -> do
                    when (n2 == 0) $ throwError $ DivideByZeroGTException pos'
                    pure $ VInt $ n1 `mod` n2
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EAdd pos e1 op e2) =
        evalBinary e1 e2 $ \case
            (VInt n1, VInt n2) -> pure $ VInt $ case op of
                OpPlus _ -> n1 + n2
                OpMinus _ -> n1 - n2
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (ERel _ e1 op e2) =
        evalBinary e1 e2 $ \(v1, v2) ->
            pure $ VBool $ case op of
                OpLT _ -> v1 < v2
                OpLE _ -> v1 <= v2
                OpGT _ -> v1 > v2
                OpGE _ -> v1 >= v2
    eval (EEq _ e1 op e2) =
        evalBinary e1 e2 $ \(v1, v2) ->
            pure $ VBool $ case op of
                OpEq _ -> v1 == v2
                OpNeq _ -> v1 /= v2
    eval (EAnd _ e1 e2) = eval e1 >>= \v1 -> if v1 == VBool True then eval e2 else pure v1
    eval (EOr _ e1 e2) = eval e1 >>= \v1 -> if v1 == VBool True then pure v1 else eval e2
    eval (EAssign _ (EIdent _ x) (OpAssign _) e) = eval e >>= \v -> modify (esUpdate x v) >> pure v
    eval (EAssign pos (EIndex _ (EIdent _ x) idx) (OpAssign _) e) = do
        -- FIXME: for now only one-dimensional arrays are supported
        arr <- gets $ esGet x
        i <- eval idx
        v <- eval e
        case (arr, i) of
            (VArray a, VInt i') -> do
                void $ checkBounds pos a i'
                modify (esUpdate x (VArray (a // [(i', v)]))) >> pure v
            _ | otherwise -> throwError $ UnknownRuntimeGTException pos
    eval (EAssign pos1 e1 (OpAssignTimes pos2) e2) = eval $ makeCompoundAssignment pos1 e1 (OpTimes pos2) e2
    eval (EAssign pos1 e1 (OpAssignDiv pos2) e2) = eval $ makeCompoundAssignment pos1 e1 (OpDiv pos2) e2
    eval (EAssign pos1 e1 (OpAssignMod pos2) e2) = eval $ makeCompoundAssignment pos1 e1 (OpMod pos2) e2
    eval (EAssign pos1 e1 (OpAssignPlus pos2) e2) = eval $ makeCompoundAssignment pos1 e1 (OpPlus pos2) e2
    eval (EAssign pos1 e1 (OpAssignMinus pos2) e2) = eval $ makeCompoundAssignment pos1 e1 (OpMinus pos2) e2
    eval (EAssign pos _ _ _) = throwError $ UnknownRuntimeGTException pos
    eval (ELambda _ args _ block) = gets (VFunc (Ident "") args block . env)
    eval (EEmpty _) = pure VVoid
