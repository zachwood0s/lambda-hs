{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import qualified Data.Map as Map
import GHC.Generics

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


class DoCodegen a where 
  codegen :: a -> Codegen AST.Operand

instance DoCodegen a => S.GProcess (K1 i a) (Codegen AST.Operand) where 
  gprocess (K1 x) = codegen x

instance DoCodegen S.Expr where 
  codegen expr = S.process expr 
-}

{-
instance DoCodegen S.MkClosure where 
  codegen (S.MkClosure name lambda env) = 
    define double name fnarg bls 
    where 
      fnarg = (double, AST.Name (S._param lambda))   -- TODO: Change with type system
      bls = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName 
        setBlock entry 
        var <- alloca double -- TODO: Change with type system 
        store var (local (AST.Name a))
        assign a var 
        codegen lambda >>= ret

instance DoCodegen S.Lambda where 
  codegen (S.Lambda env param body) = codegen body

instance DoCodegen S.Literal where 
  codegen (S.Float n) = return $ cons $ C.Float (F.Double n)
  -- TODO: Add other literal

    


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


codegenMod :: AST.Module -> [S.Expr] -> IO AST.Module
codegenMod mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn = mapM (codegenTop . transforms) fns
    newast = runLLVM mod modn

        -}
transforms :: S.Expr -> S.Expr
transforms = closureConvert
