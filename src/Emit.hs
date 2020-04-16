{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, DefaultSignatures #-}
module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.ByteString.UTF8 as BU
import Data.Word
import Data.Int
import Control.Monad.Except
import qualified Data.Map as Map
import GHC.Generics
import Control.Monad.Except

import ClosureConvert
import Codegen
import qualified AST as S


{-
codegenTop (S.Abstraction name body) = do
  define double name fnargs bls
  where 
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
	var <- alloca double
	store var (local (AST.Name a))
	assign a var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where 
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Var x) = getvar >>= load
cgen (S.Application fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs




instance DoCodegen S.Expr where 
  codegen expr = S.process expr 
-}

instance DoCodegen a => S.GProcess (K1 i a) (Codegen AST.Operand) where 
  gprocess (K1 x) = codegen x

class DoCodegen a where 
  codegen :: a -> Codegen AST.Operand
  default codegen :: (Generic a, S.GProcess (Rep a) (Codegen AST.Operand))
                  => a -> Codegen AST.Operand
  codegen = S.process

instance DoCodegen S.Expr -- Uses generic instance

instance DoCodegen S.Assign where 
  codegen (S.Assign _ e) = codegen e

instance DoCodegen S.Var where 
  codegen (S.Var x) = getvar x >>= load
  codegen (S.EnvRef _ x) = getvar x >>= load -- TODO: update this to new type

instance DoCodegen S.App where 
  codegen (S.App a _) = codegen a -- TODO: make exception stuff
  codegen (S.AppC a b) = do 
    codegen a 
    codegen b 

instance DoCodegen S.Abstraction -- Uses generic instance

instance DoCodegen S.MkClosure where 
  codegen (S.ClosureRef x) = return $ cons $ C.Int 0 0
  codegen _ = return $ cons $ C.Int 0 0

instance DoCodegen S.Lambda where 
  codegen (S.Lambda env param body) = codegen body

instance DoCodegen S.Literal where 
  codegen (S.Float n) = return $ cons $ C.Float (F.Double n)

{-





instance DoCodegen S.MkClosure where 
  codegen (S.MkClosure name lambda env) = 
    define double name [fnarg] bls 
    where 
      fnarg = (double, AST.Name $ strToBS (S._param lambda))   -- TODO: Change with type system
      bls = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName 
        setBlock entry 
        forM args $ \a -> do
          var <- alloca double
          store var (local (AST.Name $ strToBS a))
          assign a var
        codegen lambda >>= ret

instance DoCodegen S.Lambda where 
  codegen (S.Lambda env param body) = codegen body

instance DoCodegen S.Literal where 
  codegen (S.Float n) = return $ cons $ C.Float (F.Double n)
  -- TODO: Add other literal
-}

codegenClosure :: S.MkClosure -> LLVM (AST.Type)
codegenClosure (S.MkClosure name lambda env) = do
  envType <- mkEnv env 
  fnType <- mkLambda lambda envType
  mkClosure fnType envType
  where 
    mkClosure fn env = do 
      struct name [fn, env]
      return $ ptrToName name
    mkEnv (S.MkEnv names) = do 
      struct "fake" (map (const double) names) 
      return $ ptrToName "fake"
    mkLambda (S.Lambda _ param body) envType = do
      define double name fnargs bls 
      return $ ptrToFunc double types 
      where 
        args = ["env", param]
        types = [envType, double]
        fnargs = zip types (map AST.mkName args)
        bls = createBlocks $ execCodegen $ do 
          entry <- addBlock entryBlockName 
          setBlock entry 
          forM (zip types args) $ \(t, a) -> do
            var <- alloca t
            store var (local (AST.mkName a))
            assign a var
          codegen body >>= ret

codegenTop :: S.Declaration -> LLVM ()
codegenTop e = case e of 
  S.DFunction name closure -> codegenClosure closure >> return ()


codegenMod :: AST.Module -> [S.Expr] -> IO AST.Module
codegenMod mod fns = withContext $ \context -> do
    print $ map eliminateLambdas fns
    llstr <- withModuleFromAST context newast moduleLLVMAssembly
    putStrLn $ BU.toString llstr
    return newast
  where
    modn = mapM codegenTop (concatMap transforms fns)
    newast = runLLVM mod modn

{-


codegenTop :: S.Expr -> LLVM ()
codegenTop exp = case exp of 
  S.EAbs name body -> do 
    define double name fnargs bls 
    where 
      fnargs = toSig args 
      bls = createBlocks $ execCodegen $ do 
        entry <- addBlock entryBlockName 
        setBlock entry 
        forM args $ \a -> do
          var <- alloca double 
          store var (local (AST.Name a))
          assign a var 
        cgen body >>= ret



        -}
transforms :: S.Expr -> [S.Declaration]
transforms = eliminateLambdas
