module ClosureConvert where

import qualified Data.Set as Set
import Control.Monad.State.Lazy
import Control.Monad.Extra

type Name = String
type Env = String 
type VarSet = Set.Set Name 

data Expr 
  = ELam Name Expr 
  | EVar Name 
  | EApp Expr Expr 
  
  -- 
  | ELam' Env Name Expr 
  | EMakeClosure Expr Expr 
  | EAppClosure Expr Expr 
  | EMakeEnv [Name]
  | EnvRef Env Name
  deriving Show 

data ConversionState = ConversionState
  { freeVars :: VarSet
  , envCount :: Int
  }

emptyConversionState :: ConversionState
emptyConversionState = ConversionState Set.empty 0

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

deleteFree :: String -> State ConversionState ()
deleteFree free = modifyFreeVars (Set.delete free)

insertFree :: String -> State ConversionState ()
insertFree free = modifyFreeVars (Set.insert free)

emptyFree :: ConversionState -> ConversionState
emptyFree s = s { freeVars = Set.empty} 

isFree :: String -> State ConversionState Bool
isFree name = gets freeVars >>= return . (Set.member name)

setSingleFree :: String -> State ConversionState ()
setSingleFree n = modifyFreeVars (const $ Set.singleton n)

closureConvertM :: Expr -> State ConversionState Expr
closureConvertM (ELam param body) = do
  env <- getNextEnv
  body' <- closureConvertM body 
  deleteFree param
  body'' <- makeEnvRefCallsM env body'
  free <- gets (Set.toList . freeVars)
  return $ EMakeClosure
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

closureConvert' :: Expr -> (Expr, VarSet)
closureConvert' (ELam param body) =
  -- Convert lambda expressions below 
  -- Remove param from returned free var list 
  -- convert env access 
  -- return frevar list and new nodes
  let 
    env = "env"
    (body', free) = closureConvert' body 
    free' = Set.delete param free 
    body'' = makeEnvRefCalls env free' body'
    closure = EMakeClosure 
      (ELam' env param body'')
      (EMakeEnv $ Set.toList free')
  in 
    (closure, free')

closureConvert' e@(EVar name) =
  (e, Set.singleton name)

closureConvert' e@(EApp fun arg) = 
  let (fun', free1) = closureConvert' fun 
      (arg', free2) = closureConvert' arg 
  in
    (EAppClosure fun' arg', Set.union free1 free2)

closureConvert' e@(EAppClosure fun arg) =
  let (fun', free1) = closureConvert' fun 
      (arg', free2) = closureConvert' arg 
  in
    (EAppClosure fun' arg', Set.union free1 free2)

closureConvert' e = (e, Set.empty)

makeEnvRefCalls :: Env -> VarSet-> Expr-> Expr
makeEnvRefCalls env free e@(EVar name)
  | Set.member name free = EnvRef env name -- Located free variable. Need to use environment
  | otherwise = e 

makeEnvRefCalls env free (EApp fun arg) =
  EApp (makeEnvRefCalls env free fun) (makeEnvRefCalls env free arg)

makeEnvRefCalls env free (EAppClosure fun arg) = 
  EAppClosure (makeEnvRefCalls env free fun) (makeEnvRefCalls env free arg)

makeEnvRefCalls env free exp = exp -- Do nothing for everything else

test :: Expr
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
