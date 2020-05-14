{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, DefaultSignatures, RecursiveDo, OverloadedStrings, TupleSections #-}
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
import Data.List
import qualified Data.Set as Set
import Control.Monad.Except
import qualified Data.Map as M
import GHC.Generics
import Control.Monad.Except
import Control.Monad.State
import Debug.Trace

import ClosureConvert
import ConvertFuncAssign
import Utils
import qualified AST as S

data Env = Env 
  { _symbolTable :: M.Map S.Name AST.Operand 
  , _structs :: [S.Struct]
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

addstruct :: MonadState Env m => S.Struct -> m ()
addstruct op = 
  modify $ \env -> env { _structs = op : (_structs env) }

getFields :: MonadState Env m => S.Name -> m [S.Bind]
getFields name = do 
  ss <- gets _structs 
  case find (\s -> S.structName s == name) ss of 
    Nothing -> error $ "Internal error - struct not found: " ++ show name
    Just (S.Struct _ binds) -> pure binds

type LLVM = IR.ModuleBuilderT (State Env)
type Codegen = IR.IRBuilderT LLVM

strToBS :: String -> BS.ShortByteString
strToBS = BS.toShort . BU.fromString 

bsToStr :: BS.ShortByteString -> String 
bsToStr = BU.toString . BS.fromShort

{--------------------
Basic Type Handling
---------------------}

instance HasType a => S.GProcess (K1 i a) S.Type where 
  gprocess (K1 x) = getType x

class HasType a where 
  getType :: a -> S.Type 
  default getType :: (Generic a, S.GProcess (Rep a) S.Type) 
                  => a -> S.Type 
  getType = S.process

instance HasType S.Expr -- Uses generic instance

instance HasType S.Assign where 
  getType (S.Assign _ e) = getType e

instance HasType S.Var where 
  getType (S.Var x) = S.TyFloat
  getType (S.EnvRef env x) = S.TyFloat

instance HasType S.App where 
  getType (S.App a _) = getType a

instance HasType S.Abstraction -- Uses generic instance

instance HasType S.Function where 
  getType (S.Function _ body) = getType body

instance HasType S.MkClosure where 
  getType (S.ClosureRef closure envName) = S.TyPtr $ S.TyStruct closure

instance HasType S.Lambda where 
  getType (S.Lambda param body) = getType body

instance HasType S.Literal where 
  getType _ = S.TyFloat

{--------------------
Codegen
---------------------}

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
  codegen (S.Var x) = getvar x >>= flip IR.load 0
  codegen (S.EnvRef env x) = do 
    bindings <- getFields env
    case elemIndex x (map S.bindName bindings) of 
      Just idx -> do 
        var <- getvar "env"
        struct <- IR.load var 0
        structAccess' struct idx >>= flip IR.load 0
      Nothing -> error $ "Accessing var not in environment "++x
    

instance DoCodegen S.App where  
  -- For functions
  codegen (S.App (S.EVar (S.EnvRef _ a)) b) = do 
    arg <- fmap (, []) $ codegen b 
    func <- getvar a 
    IR.call func [arg]
    
  -- Higher order functions
  codegen (S.App (S.EVar (S.Var a)) b) = do
    op <- getvar a
    func <- structAccess' op 0 >>= flip IR.load 0
    let env = structAccess' op 1 >>= flip IR.load 0
    args <- mapM (fmap (, [])) [env, codegen b]
    IR.call func args

  -- Closures
  codegen (S.App a b) = do 
    op <- codegen a 
    func <- structAccess' op 0 >>= flip IR.load 0
    let env = structAccess' op 1 >>= flip IR.load 0
    args <- mapM (fmap (, [])) [env, codegen b]

    IR.call func args



instance DoCodegen S.Abstraction -- Uses generic instance

instance DoCodegen S.Function where 
  codegen _ = return $ IR.int8 0

instance DoCodegen S.MkClosure where 
  codegen (S.ClosureRef closure envName) = do
    -- Allocate both the closure and the environment
    c <- malloc (S.TyStruct closure)
    e <- malloc (S.TyStruct envName)

    -- Get all the environment fields and store them in the environment
    fields <- getFields envName
    mapM (storeEnvVars e) $ zip [0..] fields

    envField <- structAccess' c 1
    IR.store envField 0 e

    closField <- structAccess' c 0
    func <- getvar closure
    IR.store closField 0 func
    return c

    where 
      storeEnvVars envOp (idx, S.Bind ty var) = do
        syms <- gets _symbolTable 
        rhs <- if M.member var syms then codegen (S.Var var)
               else codegen (S.EnvRef envName var)

        --loadedRhs <- IR.load rhs 0

        lhs <- structAccess' envOp idx 

        IR.store lhs 0 rhs

  codegen _ = return $ IR.int8 0

instance DoCodegen S.Lambda where 
  codegen (S.Lambda param body) = codegen body

instance DoCodegen S.Literal where 
  codegen (S.Float n) = return $ IR.double n

malloc :: S.Type -> Codegen AST.Operand 
malloc ty = do 
  size <- sizeofOp ty 
  e <- call "malloc" [size]
  ltypeOfType (S.TyPtr ty) >>= IR.bitcast e 

structAccess :: S.Name -> Int -> Codegen AST.Operand 
structAccess name idx = getvar name >>= \e -> structAccess' e idx

structAccess' :: AST.Operand -> Int -> Codegen AST.Operand
structAccess' e idx = do
  let zero = IR.int32 0
      offset = IR.int32 (fromIntegral idx)
  IR.gep e [zero, offset]

call :: S.Name -> [AST.Operand] -> Codegen AST.Operand
call fun es = do 
  f <- getvar fun 
  let es' = map (, []) es
  IR.call f es'
  

codegenBuiltIn :: (String, [AST.Type], AST.Type) -> LLVM ()
codegenBuiltIn (name, argtys, retty) = do 
  func <- IR.extern (AST.mkName name) argtys retty 
  assign name func



builtIns :: [(String, [AST.Type], AST.Type)]
builtIns = 
  [ ("malloc", [AST.i32], AST.ptr AST.i8)
  , ("print", [AST.double], AST.double)
  ]
  

ptrToName :: S.Name -> AST.Type 
ptrToName name = AST.ptr $ AST.NamedTypeReference (AST.mkName name)

ptrToFunc :: AST.Type -> [AST.Type] -> AST.Type 
ptrToFunc ret args = AST.ptr $ AST.FunctionType ret args False

ltypeOfType :: MonadState Env m => S.Type -> m AST.Type 
ltypeOfType ty = case ty of 
  S.TyFloat -> pure AST.double
  S.TyInt -> pure AST.i32
  S.TyVoid -> pure AST.void 
  S.TyChar -> pure AST.i8
  S.TyBool -> pure AST.i1
  S.TyPtr (S.TyStruct n) -> 
    pure $ AST.ptr (AST.NamedTypeReference (AST.mkName ("struct."<>n)))
  S.TyPtr t -> fmap AST.ptr (ltypeOfType t)
  S.TyStruct n -> do 
    fields <- getFields n
    typs <- mapM (ltypeOfType . S.bindType) fields 
    pure $ AST.StructureType {AST.isPacked = True, AST.elementTypes = typs }

sizeof :: MonadState Env m => S.Type -> m Word32
sizeof ty = case ty of 
  S.TyBool -> pure 1
  S.TyChar -> pure 1 
  S.TyInt -> pure 4
  S.TyFloat -> pure 8
  S.TyVoid -> pure 0
  S.TyPtr _ -> pure 8
  S.TyStruct n -> do 
    fields <- getFields n 
    sizes <- mapM (sizeof . S.bindType) fields 
    pure (sum sizes)

sizeofOp :: S.Type -> Codegen AST.Operand 
sizeofOp ty = IR.int32 . fromIntegral <$> sizeof ty


emitTypeDef :: S.Name -> [AST.Type] -> LLVM AST.Type 
emitTypeDef name fieldTys = do
  let typ = AST.StructureType { AST.isPacked = True, AST.elementTypes = fieldTys }
  IR.typedef (AST.mkName ("struct." <> name)) (Just typ)

codegenClosure :: S.MkClosure -> LLVM (AST.Type)
codegenClosure (S.MkClosure name lambda env) = do
  mkEnv env 
  let envType = S.TyPtr $ S.TyStruct (S.structName env)
  e <- ltypeOfType envType
  mkLambda lambda envType
  let fnType = ptrToFunc AST.double [e, AST.double]
  mkClosure fnType envType
  where 
    mkClosure fn env' = do 
      ty <- ltypeOfType env'
      emitTypeDef name [fn, ty]
      addstruct $ S.Struct name 
        [ S.Bind S.TyInt "fn" 
        , S.Bind env' "env"
        ]
      return $ ptrToName name
    mkEnv (S.Struct envName names) = do 
      let envName' = envName
      emitTypeDef envName (map (const AST.double) names) 
      addstruct env
    mkLambda (S.Lambda param body) envType = mdo
      assign name function 
      function <- locally $ do 
        retty <- ltypeOfType (getType body)
        let args = [S.Bind envType "env", S.Bind S.TyFloat param]
        params <- mapM mkParam args
        IR.function name' params retty (genBody args)
      return ()
      where 
        name' = AST.mkName name
        mkParam (S.Bind t n) = (,) <$> ltypeOfType t <*> pure (IR.ParameterName $ strToBS n)
        genBody :: [S.Bind] -> [AST.Operand] -> Codegen ()
        genBody args ops = do 
          _entry <- IR.block `IR.named` "entry"
          forM_ (zip ops args) $ \(op, S.Bind _ n) -> do 
            addr <- IR.alloca (typeOf op) Nothing 0
            IR.store addr 0 op 
            assign n addr

          codegen body >>= IR.ret

codegenFunction :: S.Function -> LLVM ()
codegenFunction (S.Function name body) = do 
  mkLambda body
  where
    mkLambda (S.Lambda param body) = mdo
      assign name function 
      function <- locally $ do 
        retty <- ltypeOfType (getType body)
        let args = [S.Bind S.TyFloat param]
        params <- mapM mkParam args
        IR.function name' params retty (genBody args)
      return ()
      where 
        name' = AST.mkName name
        mkParam (S.Bind t n) = (,) <$> ltypeOfType t <*> pure (IR.ParameterName $ strToBS n)
        genBody :: [S.Bind] -> [AST.Operand] -> Codegen ()
        genBody args ops = do 
          _entry <- IR.block `IR.named` "entry"
          forM_ (zip ops args) $ \(op, S.Bind _ n) -> do 
            addr <- IR.alloca (typeOf op) Nothing 0
            IR.store addr 0 op 
            assign n addr

          codegen body >>= IR.ret
  

codegenTop :: S.Declaration -> LLVM ()
codegenTop e = case e of 
  S.DClosure name closure -> codegenClosure closure >> return ()
  S.DFunction name body -> codegenFunction body >> return ()


charStar :: AST.Type
charStar = AST.ptr AST.i8

codegenMod :: [S.Expr] -> AST.Module
codegenMod fns = 
  flip evalState (Env { _symbolTable = M.empty , _structs = [] })
    $ IR.buildModuleT "test"
    $ do 
      printf <- IR.externVarArgs (AST.mkName "printf") [charStar] AST.i32
      assign "printf" printf
      mapM_ codegenBuiltIn builtIns
      mapM_ codegenTop (transforms fns)

transforms :: [S.Expr] -> [S.Declaration]
transforms 
  = concatMap lambdaLift
  . map (convertFuncAssign)
  . closureConverts


