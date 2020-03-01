module ClosureConvert where

import qualified Data.Set as Set
import Control.Monad.State.Lazy
import Control.Monad.Extra
import Control.Monad.Identity
import Debug.Trace

type Name = String
type Env = String 
type VarSet = Set.Set Name 

data Expr 
  = ELam Name Expr 
  | EVar Name 
  | EApp Expr Expr 
  
  -- 
  | ELam' Env Name Expr 
  | EMakeClosure Name Expr Expr 
  | EAppClosure Expr Expr 
  | EMakeEnv [Name]
  | EnvRef Env Name
  deriving Show 

descendM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
descendM f e = f =<< case e of
  ELam a b    -> ELam <$> pure a <*> descendM f b
  EVar a      -> EVar <$> pure a
  EApp a b    -> EApp <$> descendM f a <*> descendM f b
  ELam' a b c -> ELam' <$> pure a <*> pure b <*> descendM f c
  EMakeClosure a b c 
              -> EMakeClosure <$> pure a <*> descendM f b <*> pure c
  EMakeEnv a  -> EMakeEnv <$> pure a
  EnvRef a b  -> EnvRef <$> pure a <*> pure b

descend :: (Expr -> Expr) -> Expr -> Expr
descend f ex = trace "build" $ runIdentity (descendM (pure . f) ex)
  

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
  startFree <- emptyFree		-- This seems bad
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

test :: Expr
test = trace "Run" $ transform
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
{-
test = closureConvert
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
  -}
