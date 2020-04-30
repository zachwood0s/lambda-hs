{-# LANGUAGE TypeOperators #-}

module ClosureConvert 
  ( closureConvert, closureConvertM
  , eliminateLambdas
  ) where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
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

makeEnvRefCalls :: Env -> VarSet -> Expr -> Expr
makeEnvRefCalls env fv = makeRecurse ops
  where 
    ops :: Expr :- Var :- BaseOp
    ops = stop :- replace :- baseOp

    stop :: Expr -> Expr 
    stop e = case e of 
      EAbs _ -> e                   -- Stop descent if we hit another lambda
      _      -> makeDescend ops e   -- Otherwise continue descent
    
    replace :: Var -> Var
    replace e = case e of 
      Var n | Set.member n fv -> EnvRef env n
      _ -> e

closureConvertM :: Expr -> ConvertM Expr
closureConvertM = applyBottomUpM2 closures app
  where 
    closures :: Abstraction -> ConvertM Abstraction
    closures abs = case abs of 
      ALambda (Lambda param body) -> do
        env <- getNextEnv 
        lam <- getNextLam 
        let fv = freeVars abs
        let body' = makeEnvRefCalls env fv body 
        return 
          $ AClosure 
            $ MkClosure lam 
              (Lambda param body')
              (Struct env $ map (Bind TyFloat) $ Set.toList fv)

      _ -> return abs

    app :: App -> ConvertM App 
    app a = case a of 
      App a b -> return $ AppC a b
      _ -> return a

closureConvert :: Expr -> Expr 
closureConvert e = evalState (closureConvertM e) emptyConvState

type LiftM = Writer [Declaration]
liftClosuresM :: Expr -> LiftM Expr
liftClosuresM = applyBottomUpM lift 
  where 
    lift :: MkClosure -> LiftM MkClosure
    lift c@(MkClosure name body env) = do
      tell [DFunction name c]
      return $ ClosureRef name (structName env) 
      where 
        updatedClosure = c { _lambda = body { _body = closureRef name (structName env)} }


execLift :: Expr -> [Declaration]
execLift e = execWriter (liftClosuresM e)

eliminateLambdas :: Expr -> [Declaration]
eliminateLambdas = execLift . closureConvert

{-
closureConvert = id
eliminateLambdas = const []
closureConvertM a = return a
-}




