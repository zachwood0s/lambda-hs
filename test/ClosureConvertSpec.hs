module ClosureConvertSpec
  ( closureConvertSpec
  )where

import Test.Tasty
import Test.Tasty.Hspec

import AST hiding (lambda)
import ClosureConvert

var :: Name -> Expr
var = EVar . Var

envRef :: Env -> Name -> Expr
envRef env var = EVar $ EnvRef env var

lambda :: Name -> Expr -> Expr
lambda param body = EAbs $ ALambda $ Lambda Nothing param body

closure :: Name -> Env -> Name -> Expr -> MkEnv -> Expr
closure name envName param body env = 
  EAbs $ AClosure $ MkClosure name (Lambda (Just envName) param body) env

-------------------------
-- Test Cases
-------------------------

simpleClosure :: Expr 
simpleClosure = lambda "x" (var "x")

simpleClosureResult :: Expr
simpleClosureResult = closure "lambda0" "env0" "x" (var "x") (MkEnv [])


freeVar :: Expr 
freeVar = lambda "x" (var "y") 

freeVarResult :: Expr
freeVarResult = 
  closure "lambda0" "env0" "x" 
    (envRef "env0" "y") 
    (MkEnv ["y"])

{-
nested :: Expr 
nested = ELam "x" (ELam "y" (EVar "x"))

nestedResult :: Expr 
nestedResult = EMkClosure "lambda1" 
  (ELam' "env1" "x"
    (EMkClosure "lambda0" 
      (ELam' "env0" "y"
        (EEnvRef "env0" "x"))
      (EMkEnv ["x"])))
  (EMkEnv [])
    
app :: Expr 
app = ELam "x" (EApp (EVar "z") (EVar "x"))

appResult :: Expr
appResult = EMkClosure "lambda0"
  (ELam' "env0" "x" 
    (EAppClosure 
      (EEnvRef "env0" "z")
      (EVar "x")))
  (EMkEnv ["z"])

complex :: Expr 
complex = ELam "x" 
  (ELam "y" 
    (EApp 
      (EApp 
        (ELam "f"
          (EVar "f"))
        (EApp 
          (EVar "f")
          (EVar "z")))
      (ELam "z" 
        (EApp 
          (EVar "z")
          (EVar "x")))))

complexResult :: Expr 
complexResult = EMkClosure "lambda3"
  (ELam' "env3" "x"
    (EMkClosure "lambda2" 
      (ELam' "env2" "y"
        (EAppClosure 
          (EAppClosure 
            (EMkClosure "lambda0" 
              (ELam' "env0" "f"
                (EVar "f"))
              (EMkEnv []))
            (EAppClosure 
              (EEnvRef "env2" "f")
              (EEnvRef "env2" "z")))
          (EMkClosure "lambda1"
            (ELam' "env1" "z"
              (EAppClosure 
                (EVar "z")
                (EEnvRef "env1" "x")))
            (EMkEnv ["x"]))))
      (EMkEnv ["f", "z"])))
    (EMkEnv [])

-}


closureConvertSpec :: Spec
closureConvertSpec = do
  describe "ClosureConvert.closureConvert" $ do
    it "makes simple closure with no env" $ do
      closureConvert simpleClosure `shouldBe` simpleClosureResult

    it "converts free variable references" $ do
      closureConvert freeVar `shouldBe` freeVarResult

{-
    it "converts nested closures" $ do 
      closureConvert nested `shouldBe` nestedResult

    it "converts applications" $ do 
      closureConvert app `shouldBe` appResult

    it "converts complex expressions" $ do 
      closureConvert complex `shouldBe` complexResult
-}
