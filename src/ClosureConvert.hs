module ClosureConvert where

import AST

import Control.Monad.State.Lazy

import qualified Data.Set as Set

data ConversionState = ConversionState
  { _envCount :: Int 
  , _lamCount :: Int 
  }

type ConvertM a = State ConversionState a

-----------------------------
-- Utils
-----------------------------

envName :: Int -> Env 
envName n = "env" ++ (show n)

lamName :: Int -> Name 
lamName n = "lambda" ++ (show n)

-----------------------------
-- ConversionState Helpers
-----------------------------

emptyConvState :: ConversionState
emptyConvState = ConversionState 0 0

getNextEnv :: ConvertM Env
getNextEnv = do 
  count <- gets _envCount 
  modify (\xs -> xs { _envCount = count + 1 })
  return $ envName count

getNextLam :: ConvertM Name 
getNextLam = do 
  count <- gets _lamCount 
  modify (\xs -> xs { _lamCount = count + 1 })
  return $ lamName count

-----------------------------
-- Conversion
-----------------------------

-- | Converts all EVar accesses who are contained
-- in the free set to EEnvRefs
makeEnvRefCalls :: Env    -- ^ env to create ref calls for
                -> VarSet -- ^ set of free vars
                -> Expr   -- ^ expression to convert
                -> Expr   -- ^ converted expression
makeEnvRefCalls env free = descend f 
  where 
    f exp = case exp of 
      EVar n 
        | Set.member n free -> EEnvRef env n
      _ -> exp

-- | Converts an ELam into EMkClosure by calculating
-- free variables, converting variable references in
-- the body, and making an environment from free vars
closureConvertM :: Expr           -- ^ expression to convert
                -> ConvertM Expr  -- ^ resulting conversion monad
closureConvertM = descendM f
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
  
testExpr1 = 
  (ELam "x" 
    (EApp
      (EApp 
        (EVar "f")
        (EVar "z")
      )
      (ELam "y"
        (EVar "j")
      )
    )
  )

testExpr2 = 
  (ELam "x" 
    (EApp 
      (ELam "y" 
        (EApp 
          (EVar "y")
          (EVar "x")
        )
      )
      (EApp 
        (EVar "f")
        (EVar "z")
      )
    )
  )

test :: Expr
test = closureConvert testExpr2


{-
data ConversionState = ConversionState
  { freeVars :: VarSet
  , envCount :: Int
  , lambdaCount :: Int
  }

emptyConversionState :: ConversionState
emptyConversionState = ConversionState Set.empty 0 0

lambdaName :: Int -> String
lambdaName n = "lambda" ++ (show n)

envName :: Int -> String
envName n = "env" ++ (show n)

modifyFreeVars :: (VarSet -> VarSet) -> State ConversionState ()
modifyFreeVars f = 
  modify (\xs -> xs {freeVars = f (freeVars xs) })

getNextEnv :: State ConversionState String
getNextEnv = do 
  count <- gets envCount
  modify (\xs -> xs { envCount = count + 1 })
  return $ envName count

getNextLambda :: State ConversionState String
getNextLambda = do
  count <- gets lambdaCount
  modify (\xs -> xs { lambdaCount = count + 1 })
  return $ lambdaName count

emptyFree :: State ConversionState VarSet
emptyFree = do 
  startFree <- gets freeVars
  modifyFreeVars (const Set.empty)
  return startFree

deleteFree :: String -> State ConversionState ()
deleteFree free = modifyFreeVars (Set.delete free)

insertFree :: String -> State ConversionState ()
insertFree free = modifyFreeVars (Set.insert free)

isFree :: String -> State ConversionState Bool
isFree name = gets freeVars >>= return . (Set.member name)

setSingleFree :: String -> State ConversionState ()
setSingleFree n = modifyFreeVars (const $ Set.singleton n)

closureConvertM :: Expr -> State ConversionState Expr
closureConvertM (ELam param body) = do
  startFree <- emptyFree    -- This seems bad
          -- I'm essentially keeping a global state
          -- But clearing it at every recursive step
          -- I don't think I actually need that
  env <- getNextEnv
  lam <- getNextLambda
  body' <- closureConvertM body 
  deleteFree param
  body'' <- makeEnvRefCallsM env body'
  free <- gets (Set.toList . freeVars)
  modifyFreeVars (const startFree)  
  return $ EMakeClosure lam
    (ELam' env param body'')
    (EMakeEnv free)

closureConvertM exp = case exp of 
  EVar name -> insertFree name >> return exp
  EApp fun arg -> convertApp fun arg
  EAppClosure fun arg -> convertApp fun arg
  _ -> return exp
  where 
    convertApp :: Expr -> Expr -> State ConversionState Expr
    convertApp fun arg = 
      EAppClosure <$> closureConvertM fun <*> closureConvertM arg

makeEnvRefCallsM :: Env -> Expr -> State ConversionState Expr
makeEnvRefCallsM env e = case e of 
  EVar name ->
    ifM (isFree name) (return $ EnvRef env name) (return e)
  EApp fun arg ->
    EApp <$> makeEnvRefCallsM env fun <*> makeEnvRefCallsM env arg
  EAppClosure fun arg ->
    EAppClosure <$> makeEnvRefCallsM env fun <*> makeEnvRefCallsM env arg
  _ -> return e

closureConvert :: Expr -> Expr 
closureConvert e = evalState (closureConvertM e) $ emptyConversionState

transform :: Expr -> Expr
transform = descend f
  where 
    f e@(ELam _ x) = EMakeClosure "Converted" e (EMakeEnv ["free"])
    f (EVar "f") = EVar "lkjslkj"
    f x = x

    -}
