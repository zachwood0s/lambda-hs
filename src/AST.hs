{-# LANGUAGE DeriveDataTypeable #-}

module AST where

import Data.Data 
import Data.Typeable
import Data.Set ((\\))
import qualified Data.Set as Set

type Name = String
type Env = String 
type VarSet = Set.Set Name

data Expr 
  = ELit Literal 
  | EAbs Abstraction
  | EApp App 
  | EVar Var
  | EAssign Assign
  deriving (Show, Eq, Ord, Typeable, Data)

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

data Abstraction = AClosure MkClosure | ALambda Lambda
  deriving (Show, Eq, Ord, Typeable, Data)

data MkClosure = MkClosure 
  { _name :: Name 
  , _lambda :: Lambda 
  , _env :: MkEnv
  } deriving (Show, Eq, Ord, Typeable, Data)
  
data Lambda = Lambda
  { _envName :: Maybe Env
  , _param :: Name 
  , _body :: Expr 
  } deriving (Show, Eq, Ord, Typeable, Data)

data MkEnv = MkEnv 
  { _bindings :: [Name]
  } deriving (Show, Eq, Ord, Typeable, Data)
  
data Literal 
  = Float Double
  | Int Integer 
  | Char Char
  deriving (Show, Eq, Ord, Typeable, Data)


-------------------------
-- AllVars
-------------------------

class AllVars a where 
  allVars :: a -> VarSet

class FreeVars a where 
  freeVars :: a -> VarSet

instance AllVars Expr where
  allVars (ELit a)    = allVars a
  allVars (EAbs a)    = allVars a
  allVars (EApp a)    = allVars a
  allVars (EVar a)    = allVars a
  allVars (EAssign a) = allVars a

instance AllVars Assign where 
  allVars (Assign _ e) = allVars e

instance AllVars Var where 
  allVars (Var x) = Set.singleton x
  allVars _       = Set.empty

instance AllVars App where 
  allVars (App a b)  = allVars a `Set.union` allVars b
  allVars (AppC a b) = allVars a `Set.union` allVars b

instance AllVars Abstraction where 
  allVars (AClosure c) = allVars c
  allVars (ALambda c)  = allVars c

instance AllVars MkClosure where 
  allVars (MkClosure _ e _) = allVars e

instance AllVars Lambda where 
  allVars (Lambda _ _ e) = allVars e

instance AllVars Literal where 
  allVars = const Set.empty

-------------------------
-- FreeVars
-------------------------

instance FreeVars Expr where
  freeVars (ELit a)    = freeVars a
  freeVars (EAbs a)    = freeVars a
  freeVars (EApp a)    = freeVars a
  freeVars (EVar a)    = freeVars a
  freeVars (EAssign a) = freeVars a

instance FreeVars Assign where 
  freeVars (Assign n e) = freeVars e \\ Set.singleton n

instance FreeVars Var where 
  freeVars (Var x) = Set.singleton x
  freeVars _       = Set.empty

instance FreeVars App where 
  freeVars (App a b)  = freeVars a `Set.union` freeVars b
  freeVars (AppC a b) = freeVars a `Set.union` freeVars b

instance FreeVars Abstraction where 
  freeVars (AClosure c) = freeVars c
  freeVars (ALambda c)  = freeVars c

instance FreeVars MkClosure where 
  freeVars (MkClosure _ e _) = freeVars e

instance FreeVars Lambda where 
  freeVars (Lambda _ n e) = freeVars e \\ Set.singleton n

instance FreeVars Literal where 
  freeVars = const Set.empty

-------------------------
-- Helper Functions
-------------------------

var :: Name -> Expr
var = EVar . Var

envRef :: Env -> Name -> Expr
envRef env var = EVar $ EnvRef env var

lambda :: Name -> Expr -> Expr
lambda param body = EAbs $ ALambda $ Lambda Nothing param body

closure :: Name -> Env -> Name -> Expr -> MkEnv -> Expr
closure name envName param body env = 
  EAbs $ AClosure $ MkClosure name (Lambda (Just envName) param body) env

app :: Expr -> Expr -> Expr
app a b = EApp $ App a b

appC :: Expr -> Expr -> Expr
appC a b = EApp $ AppC a b

