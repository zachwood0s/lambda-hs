{-# LANGUAGE DeriveDataTypeable #-}

module AST where

import Data.Data 
import Data.Typeable
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
  { binding :: Name 
  , value :: Expr 
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
  { name :: Name 
  , lambda :: Lambda 
  , env :: MkEnv
  } deriving (Show, Eq, Ord, Typeable, Data)
  
data Lambda = Lambda
  { envName :: Maybe Env
  , param :: Name 
  , body :: Expr 
  } deriving (Show, Eq, Ord, Typeable, Data)

data MkEnv = MkEnv 
  { bindings :: [Name]
  } deriving (Show, Eq, Ord, Typeable, Data)
  
data Literal 
  = Float Double
  | Int Integer 
  | Char Char
  deriving (Show, Eq, Ord, Typeable, Data)


