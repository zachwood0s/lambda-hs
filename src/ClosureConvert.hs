{-# LANGUAGE TypeOperators #-}

module ClosureConvert 
  ( closureConvert, closureConvertM
  , closureConverts
  , lambdaLift
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

builtIns = Set.fromList
  ["print"]

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
closureConvertM = applyBottomUpM closures
  where 
    closures :: Abstraction -> ConvertM Abstraction
    closures abs = case abs of 
      ALambda (Lambda param body) -> do
        env <- getNextEnv 
        lam <- getNextLam 
        let fv = freeVars abs
        let body' = makeEnvRefCalls env fv body 
        let filtered = filter (flip Set.notMember builtIns) (Set.toList fv)
        return 
          $ AClosure 
            $ MkClosure lam 
              (Lambda param body')
              (Struct env $ map (Bind TyFloat) filtered)

      _ -> return abs

closureConvert :: Expr -> Expr 
closureConvert e = evalState (closureConvertM e) emptyConvState

closureConverts :: [Expr] -> [Expr]
closureConverts es = evalState (mapM closureConvertM es) emptyConvState

type LiftM = Writer [Declaration]
liftClosuresM :: Expr -> LiftM Expr
liftClosuresM = applyBottomUpM2 liftC liftF
  where 
    liftC :: MkClosure -> LiftM MkClosure
    liftC c@(MkClosure name body env) = do
      tell [DClosure name c]
      return $ ClosureRef name (structName env) 
      where 
        updatedClosure = c { _lambda = body { _body = closureRef name (structName env)} }
    liftF :: Function -> LiftM Function
    liftF c@(Function name body) = do
      tell [DFunction name c]
      return c



lambdaLift :: Expr -> [Declaration]
lambdaLift e = execWriter (liftClosuresM e)

--eliminateLambdas :: Expr -> [Declaration]
--eliminateLambdas = execLift . closureConvert

{-
closureConvert = id
eliminateLambdas = const []
closureConvertM a = return a
-}




