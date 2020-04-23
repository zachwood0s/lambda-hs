{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, DefaultSignatures, RecursiveDo, OverloadedStrings #-}
module Emit where

import LLVM.Module
import LLVM.Context
import LLVM.AST.Typed (typeOf)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Monad as IR
import qualified LLVM.IRBuilder.Module as IR
import qualified LLVM.IRBuilder.Constant as IR

import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Short as BS
import Data.Word
import Data.Int
import Control.Monad.Except
import qualified Data.Map as M
import GHC.Generics
import Control.Monad.Except
import Control.Monad.State

import ClosureConvert
import Utils
import qualified AST as S

data Env = Env 
  { _symbolTable :: M.Map S.Name AST.Operand 
  } deriving (Eq, Show)

assign :: MonadState Env m => S.Name -> AST.Operand -> m ()
assign name op = 
  modify $ \env -> env { _symbolTable = M.insert name op (_symbolTable env) }

getvar :: MonadState Env m => S.Name -> m (AST.Operand)
getvar var = do 
  syms <- gets _symbolTable
  case M.lookup var syms of 
    Just x -> return x 
    nothing -> error $ "Local variable not in scope: " ++ show var

type LLVM = IR.ModuleBuilderT (State Env)
type Codegen = IR.IRBuilderT LLVM

strToBS :: String -> BS.ShortByteString
strToBS = BS.toShort . BU.fromString 

bsToStr :: BS.ShortByteString -> String 
bsToStr = BU.toString . BS.fromShort


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
cgen (S.Var x) = getvar >>= (flip load 0)
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
  codegen (S.Var x) = getvar x 
  codegen (S.EnvRef _ x) = getvar x  -- TODO: update this to new type

instance DoCodegen S.App where 
  codegen (S.App a _) = codegen a -- TODO: make exception stuff
  codegen (S.AppC a b) = do 
    codegen a 
    codegen b 

instance DoCodegen S.Abstraction -- Uses generic instance

instance DoCodegen S.MkClosure where 
  codegen (S.ClosureRef x) = return $ IR.int8 0
  codegen _ = return $ IR.int8 0

instance DoCodegen S.Lambda where 
  codegen (S.Lambda param body) = codegen body

instance DoCodegen S.Literal where 
  codegen (S.Float n) = return $ IR.double n

{-

storeEnv :: S.MkEnv -> AST.Operand -> Codegen AST.Operand 
storeEnv (S.MkEnv _ bindings) ptr = mapM storeMember (zip [1..] bindings)
  where 
    storeMember (idx, name) = do 
      val <- structAccess idx ptr
      store val (local (AST.Name name))
      -}


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

ptrToName :: S.Name -> AST.Type 
ptrToName name = AST.ptr $ AST.NamedTypeReference (AST.mkName name)

ptrToFunc :: AST.Type -> [AST.Type] -> AST.Type 
ptrToFunc ret args = AST.ptr $ AST.FunctionType ret args False

ltypeOfType :: MonadState Env m => m AST.Type 
ltypeOfType = pure AST.double


emitTypeDef :: S.Name -> [AST.Type] -> LLVM AST.Type 
emitTypeDef name fieldTys = do
  let typ = AST.StructureType { AST.isPacked = True, AST.elementTypes = fieldTys }
  IR.typedef (AST.mkName ("struct." <> name)) (Just typ)

codegenClosure :: S.MkClosure -> LLVM (AST.Type)
codegenClosure (S.MkClosure name lambda env) = do
  envType <- mkEnv env 
  mkLambda lambda envType
  let fnType = ptrToFunc AST.double [envType, AST.double]
  mkClosure fnType envType
  where 
    mkClosure fn env = do 
      emitTypeDef name [fn, env]
      return $ ptrToName name
    mkEnv (S.MkEnv env names) = do 
      emitTypeDef env (map (const AST.double) names) 
      return $ ptrToName env
    mkLambda (S.Lambda param body) envType = mdo
      assign name function 
      function <- locally $ do 
        retty <- ltypeOfType 
        IR.function (AST.mkName name) fnargs retty genBody
      return ()
      where 
        args = ["env", param]
        types = [envType, AST.double]
        fnargs = zip types (map (IR.ParameterName . strToBS) args)
        genBody :: [AST.Operand] -> Codegen ()
        genBody ops = do 
          _entry <- IR.block `IR.named` "entry"
          forM_ (zip ops args) $ \(op, n) -> do 
            addr <- IR.alloca (typeOf op) Nothing 0
            IR.store addr 0 op 
            assign n addr

          codegen body >> return ()

codegenTop :: S.Declaration -> LLVM ()
codegenTop e = case e of 
  S.DFunction name closure -> codegenClosure closure >> return ()

codegenMod :: [S.Expr] -> AST.Module
codegenMod fns = 
  flip evalState (Env { _symbolTable = M.empty })
    $ IR.buildModuleT "test"
    $ do 
      mapM_ codegenTop (concatMap transforms fns)

{-
runLLVM :: AST.Module -> IR.IRBuilderT a m -> AST.Module 
runLLVM mod (IR.IRBuilderT m) = execState m mod

codegenMod :: AST.Module -> [S.Expr] -> IO AST.Module
codegenMod mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
    -}
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
