module AST where 

import Control.Monad.Identity
import Data.Set ((\\))
import qualified Data.Set as Set

type Name = String 
type Env = String
type VarSet = Set.Set Name

data Expr 
  = ELit Literal
  | ELam Name Expr 
  | EVar String 
  | EApp Expr Expr
  | EAssign Name Expr

  -- Converted closures
  | ELam' Env Name Expr 
  | EMkClosure Name Expr Expr 
  | EAppClosure Expr Expr 
  | EMkEnv [Name]
  | EEnvRef Env Name
  deriving (Eq, Ord, Show)

data Literal
  = LFloat Double
  | LInt Integer 
  | LChar Char
  deriving (Eq, Ord, Show)

----------------------
-- All/Free Vars
----------------------

class AllVars a where 
  allVars :: a -> VarSet

class FreeVars a where 
  freeVars :: a -> VarSet

instance AllVars Expr where 
  allVars ex = case ex of 
    ELit _           -> Set.empty
    ELam _ e         -> allVars e
    EVar x           -> Set.singleton x 
    EApp a b         -> allVars a `Set.union` allVars b
    EAssign _ e      -> allVars e
    ELam' _ _ e      -> allVars e 
    EMkClosure _ e _ -> allVars e
    EAppClosure a b  -> allVars a `Set.union` allVars b
    EMkEnv _         -> Set.empty
    EEnvRef _ _      -> Set.empty

instance FreeVars Expr where 
  freeVars ex = case ex of 
    ELit _           -> Set.empty 
    ELam n e         -> freeVars e \\ Set.singleton n
    EVar x           -> Set.singleton x 
    EApp a b         -> freeVars a `Set.union` freeVars b
    EAssign n e      -> freeVars e \\ Set.singleton n
    ELam' _ n e      -> freeVars e \\ Set.singleton n
    EMkClosure _ e _ -> freeVars e
    EAppClosure a b  -> freeVars a `Set.union` freeVars b
    EMkEnv _         -> Set.empty 
    EEnvRef _ _      -> Set.empty

----------------------
-- AST Utils
----------------------

topDownM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
topDownM f ex = f ex >>= traverseM (topDownM f)

topDown :: (Expr -> Expr) -> Expr -> Expr
topDown f ex = runIdentity (topDownM (pure . f) ex)

bottomUpM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
bottomUpM f ex = traverseM (bottomUpM f) ex >>= f

bottomUp :: (Expr -> Expr) -> Expr -> Expr
bottomUp f ex = runIdentity (bottomUpM (pure . f) ex)

traverseM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
traverseM f ex = case ex of
  ELit l           -> ELit <$> pure l
  ELam a b         -> ELam <$> pure a <*> f b
  EVar a           -> EVar <$> pure a
  EApp a b         -> EApp <$> f a <*> f b
  ELam' a b c      -> ELam' <$> pure a <*> pure b <*> f c
  EMkClosure a b c -> EMkClosure <$> pure a <*> f b <*> pure c
  EAppClosure a b  -> EAppClosure <$> f a <*> f b
  EMkEnv a         -> EMkEnv <$> pure a
  EEnvRef a b      -> EEnvRef <$> pure a <*> pure b
  


