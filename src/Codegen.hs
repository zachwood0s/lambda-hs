{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Short as BS
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Type (ptr)
import LLVM.AST.Global
import qualified LLVM.AST as AST 

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

import Debug.Trace

------------------------
-- Utilities 
------------------------

strToBS :: String -> BS.ShortByteString
strToBS = BS.toShort . BU.fromString

bsToStr :: BS.ShortByteString -> String
bsToStr = BU.toString . BS.fromShort

type SymbolTable = [(String, Operand)]
type Names = Map.Map String Int

data CodegenState = CodegenState 
  { currentBlock :: Name 
  , blocks :: Map.Map Name BlockState 
  , symtab :: SymbolTable
  , blockCount :: Int 
  , count :: Word 
  , names :: Names 
  } deriving Show

data BlockState = BlockState 
  { idx :: Int 
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = strToBS label }

addDefn :: Definition -> LLVM ()
addDefn d = do 
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $ 
  GlobalDefinition $ functionDefaults 
    { name        = Name $ strToBS label 
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty 
    , basicBlocks = body
    }

struct :: String -> [Type] -> LLVM ()
struct name types = addDefn $ 
  TypeDefinition (Name $ strToBS name) 
    $ Just $ StructureType False types

fresh :: Codegen Word 
fresh = do 
  i <- gets count 
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = 
  case Map.lookup nm ns of 
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

local :: Name -> Operand 
local = LocalReference double 

externf :: Name -> Operand 
externf = ConstantOperand . C.GlobalReference double

assign :: String -> Operand -> Codegen ()
assign var x = do 
  lcls <- gets symtab 
  modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: String -> Codegen Operand 
getvar var = do 
  syms <- gets symtab 
  case lookup var syms of 
    Just x -> return x 
    Nothing -> error $ "Local variable not in scope: " ++ show var


instr :: Instruction -> Codegen Operand
instr ins = do 
  n <- fresh 
  let ref = UnName n
  blk <- current 
  let i = stack blk 
  modifyBlock (blk {stack = (ref := ins) : i})
  return $ local ref

unnminstr :: Instruction -> Codegen () 
unnminstr ins = do 
  blk <- current 
  let i = stack blk 
  modifyBlock (blk {stack = (Do ins) : i})

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do 
  blk <- current 
  modifyBlock (blk { term = Just trm })
  return trm

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []


------------------------
-- Blocks 
------------------------

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where 
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name $ strToBS entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen



------------------------
-- Block Stack
------------------------

entry :: Codegen Name 
entry = gets currentBlock 

addBlock :: String ->  Codegen Name 
addBlock bname = do 
  bls <- gets blocks 
  ix <- gets blockCount 
  nms <- gets names 

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms 
  
  modify $ \s -> s 
    { blocks = Map.insert (Name $ strToBS qname) new bls 
    , blockCount = ix + 1 
    , names = supply
    }
  
  return (Name $ strToBS qname)

setBlock :: Name -> Codegen Name 
setBlock bname = do 
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name 
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen () 
modifyBlock new = do 
  active <- gets currentBlock 
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState 
current = do 
  c <- gets currentBlock 
  blks <- gets blocks 
  case Map.lookup c blks of 
    Just x -> return x 
    Nothing -> error $ "No such block: " ++ show c

double :: Type 
double = FloatingPointType DoubleFP

ptrToName :: String -> Type 
ptrToName name = ptr $ NamedTypeReference (mkName name)

ptrToFunc :: Type -> [Type] -> Type 
ptrToFunc ret args = ptr $ FunctionType ret args False

cons :: C.Constant -> Operand 
cons = ConstantOperand

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))
