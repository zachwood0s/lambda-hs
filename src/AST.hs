{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, DefaultSignatures #-}

module AST where

import Data.Data 
import Data.Typeable
import Data.Set ((\\))
import GHC.Generics

import qualified Data.Set as Set

type Name = String
type Env = String 
type VarSet = Set.Set Name

newtype AllVarSet = All { unAllSet :: VarSet }

data Type 
  = TyBool
  | TyChar 
  | TyInt
  | TyFloat
  | TyVoid 
  | TyPtr Type
  | TyStruct Name
  deriving (Show, Eq, Ord, Typeable, Data)

data Bind = Bind { bindType :: Type, bindName :: Name }
  deriving (Show, Eq, Ord, Typeable, Data)

data Struct = Struct { structName :: Name, structFields :: [Bind] }
  deriving (Show, Eq, Ord, Typeable, Data)

data Module = Module 
  { declarations :: [Declaration]
  , name :: Name
  } deriving (Show, Eq, Ord, Typeable, Data)

data Declaration
  = DFunction Name MkClosure
  deriving (Show, Eq, Ord, Typeable, Data)

data Expr 
  = ELit Literal 
  | EAbs Abstraction
  | EApp App 
  | EVar Var
  | EAssign Assign
  deriving (Show, Eq, Ord, Typeable, Data, Generic)

data Assign = Assign 
  { _binding :: Name 
  , _value :: Expr 
  } deriving (Show, Eq, Ord, Typeable, Data)

data Var 
  = Var String
  | EnvRef Env String
  deriving (Show, Eq, Ord, Typeable, Data)

data App
  = App Expr Expr 
  | AppC Expr Expr
  deriving (Show, Eq, Ord, Typeable, Data)

data Abstraction 
  = AClosure MkClosure 
  | ALambda Lambda
  deriving (Show, Eq, Ord, Typeable, Data, Generic)

data MkClosure = MkClosure 
  { _name :: Name 
  , _lambda :: Lambda 
  , _env :: Struct
  } | 
  ClosureRef Name Env
  deriving (Show, Eq, Ord, Typeable, Data)
  
data Lambda = Lambda
  { _param :: Name 
  , _body :: Expr 
  } deriving (Show, Eq, Ord, Typeable, Data)
  
data Literal 
  = Float Double
  | Int Integer 
  | Char Char
  deriving (Show, Eq, Ord, Typeable, Data)

-------------------------
-- Generic Instances
-------------------------

class GProcess f r where 
  gprocess :: f p -> r

instance GProcess f r => GProcess (M1 i c f) r where 
  gprocess (M1 x) = gprocess x

instance (GProcess f r, GProcess g r) => GProcess (f :+: g) r where 
  gprocess (L1 x) = gprocess x
  gprocess (R1 y) = gprocess y

process :: (Generic a, GProcess (Rep a) r) => a -> r
process = gprocess . from


-------------------------
-- AllVars
-------------------------

getAllVars :: (AllVars a) => a -> VarSet
getAllVars a = unAllSet $ allVars a

class AllVars a where 
  allVars :: a -> AllVarSet
  default allVars :: (Generic a, GProcess (Rep a) AllVarSet) => a -> AllVarSet
  allVars = process

instance AllVars a => GProcess (K1 i a) AllVarSet where 
  gprocess (K1 x) = allVars x

instance AllVars Expr -- Uses generic instance

instance AllVars Assign where 
  allVars (Assign _ e) = allVars e

instance AllVars Var where 
  allVars (Var x) = All $ Set.singleton x
  allVars _       = All $ Set.empty

instance AllVars App where 
  allVars (App a b)  = All $ getAllVars a `Set.union` getAllVars b
  allVars (AppC a b) = All $ getAllVars a `Set.union` getAllVars b

instance AllVars Abstraction -- Uses generic instance

instance AllVars MkClosure where 
  allVars (MkClosure _ e _) = allVars e

instance AllVars Lambda where 
  allVars (Lambda _ e) = allVars e

instance AllVars Literal where 
  allVars = const (All Set.empty)

-------------------------
-- FreeVars
-------------------------

class FreeVars a where 
  freeVars :: a -> VarSet
  default freeVars :: (Generic a, GProcess (Rep a) VarSet) => a -> VarSet
  freeVars = process

instance FreeVars a => GProcess (K1 i a) VarSet where 
  gprocess (K1 x) = freeVars x

instance FreeVars Expr -- Uses generic instance

instance FreeVars Assign where 
  freeVars (Assign n e) = freeVars e \\ Set.singleton n

instance FreeVars Var where 
  freeVars (Var x) = Set.singleton x
  freeVars _       = Set.empty

instance FreeVars App where 
  freeVars (App a b)  = freeVars a `Set.union` freeVars b
  freeVars (AppC a b) = freeVars a `Set.union` freeVars b

instance FreeVars Abstraction -- Uses generic instance

instance FreeVars MkClosure where 
  freeVars (MkClosure _ e _) = freeVars e

instance FreeVars Lambda where 
  freeVars (Lambda n e) = freeVars e \\ Set.singleton n

instance FreeVars Literal where 
  freeVars = const Set.empty

-------------------------
-- Helper Functions
-------------------------

var :: Name -> Expr
var = EVar . Var

closureRef :: Name -> Env -> Expr
closureRef closure env = EAbs $ AClosure $ ClosureRef closure env

envRef :: Env -> Name -> Expr
envRef env var = EVar $ EnvRef env var

lambda :: Name -> Expr -> Expr
lambda param body = EAbs $ ALambda $ Lambda param body

closure :: Name -> Env -> Name -> Expr -> Struct -> Expr
closure name envName param body env = 
  EAbs $ AClosure $ MkClosure name (Lambda param body) env

app :: Expr -> Expr -> Expr
app a b = EApp $ App a b

appC :: Expr -> Expr -> Expr
appC a b = EApp $ AppC a b

