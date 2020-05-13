{-# LANGUAGE TypeOperators #-}

module ConvertFuncAssign 
  ( convertFuncAssign
  ) where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Generics.Alloy
import qualified Data.Set as Set

import AST
import ASTInstances

-----------------------------
-- Conversion
-----------------------------

renameFuncAssign :: Assign -> Assign
renameFuncAssign (Assign name (EAbs (AClosure (MkClosure _ body env)))) =
  Assign name (EAbs (AClosure (MkClosure name body env)))
renameFuncAssign e = e


convertFuncAssign :: Expr -> Expr 
convertFuncAssign = applyBottomUp renameFuncAssign

