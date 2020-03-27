{-# LANGUAGE TypeOperators #-}

module ClosureConvert 
  ( closureConvert, closureConvertM
  ) where

import Control.Monad.State.Lazy
import Data.Generics.Alloy
import qualified Data.Set as Set

import AST
import ASTInstances

-- | Values used in the closure conversion process. 
data ConversionState = ConversionState
  { _envCount :: Int -- ^ environment count
  , _lamCount :: Int -- ^ lambda count 
  }

type ConvertM a = State ConversionState a

-----------------------------
-- Utils
-----------------------------

-- | Creates an environment name from id
makeEnvName :: Int -- ^ environment id
            -> Env -- ^ created name 
makeEnvName n = "env" ++ (show n)

-- | Creates a lambda name from id
makeLamName :: Int  -- ^ lambda id
            -> Name -- ^ created name 
makeLamName n = "lambda" ++ (show n)

-----------------------------
-- ConversionState Helpers
-----------------------------

-- | Empty conversion state
emptyConvState :: ConversionState
emptyConvState = ConversionState 0 0

-- | Retreives the next environment name from
-- the conversion state and increments the 
-- environment count
getNextEnv :: ConvertM Env
getNextEnv = do 
  count <- gets _envCount 
  modify (\xs -> xs { _envCount = count + 1 })
  return $ makeEnvName count

-- | Retreives the next lambda name from
-- the conversion state and increments the 
-- lambda count
getNextLam :: ConvertM Name 
getNextLam = do 
  count <- gets _lamCount 
  modify (\xs -> xs { _lamCount = count + 1 })
  return $ makeLamName count

-----------------------------
-- Conversion
-----------------------------

closureConvertM :: Abstraction -> ConvertM Abstraction
closureConvertM exp = case exp of 
  ALambda (Lambda _ param body) -> do
    env <- getNextEnv 
    lam <- getNextLam 
    return 
      $ AClosure 
        $ MkClosure lam 
          (Lambda (Just env) param body)
          (MkEnv [])
  _ -> return exp

closureConvert :: Expr -> Expr 
closureConvert e = evalState (makeRecurseM ops e) emptyConvState
  where 
    ops :: (Abstraction :-* BaseOpA) (State ConversionState)
    ops = closureConvertM :-* baseOpA



{-
makeEnvRefCalls :: Env -> VarSet -> Expr -> Expr 
makeEnvRefCalls env fv e = f e
  where 
    f exp = case exp of 
      EVar n 
        | Set.member n fv -> EEnvRef env n
      EApp a b -> EApp (f a) (f b)
      EAppClosure a b-> EAppClosure (f a) (f b)
      _ -> exp

-- | Converts an ELam into EMkClosure by calculating
-- free variables, converting variable references in
-- the body, and making an environment from free vars
closureConvertM :: Expr           -- ^ expression to convert
                -> ConvertM Expr  -- ^ resulting conversion monad
closureConvertM = bottomUpM f
  where 
    f exp = case exp of 
      ELam n body -> do 
        env <- getNextEnv
        lam <- getNextLam
        let fv = freeVars exp
        let body' = makeEnvRefCalls env fv body 
        return $ EMkClosure lam
          (ELam' env n body')
          (EMkEnv $ Set.toList fv)

      EApp a b -> return $ EAppClosure a b
      _ -> return exp

-- | Evaluates the conversion monad. 
-- See 'closureConvertM'
closureConvert :: Expr -- ^ expression to convert
               -> Expr -- ^ converted expression 
closureConvert e = evalState (closureConvertM e) emptyConvState


-}
