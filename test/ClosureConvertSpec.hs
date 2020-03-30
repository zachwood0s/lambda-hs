module ClosureConvertSpec
  ( closureConvertSpec
  )where

import Test.Tasty
import Test.Tasty.Hspec

import AST 
import ClosureConvert


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

nested :: Expr 
nested = lambda "x" (lambda "y" (var "x"))

nestedResult :: Expr 
nestedResult = 
  closure "lambda1" "env1" "x"
    (closure "lambda0" "env0" "y"
        (envRef "env0" "x")
        (MkEnv ["x"]))
  (MkEnv [])
    
application :: Expr 
application = lambda "x" (app (var "z") (var "x"))

applicationResult :: Expr
applicationResult = 
  closure "lambda0" "env0" "x" 
    (appC 
      (envRef "env0" "z")
      (var "x"))
    (MkEnv ["z"])

complex :: Expr 
complex = lambda "x" 
  (lambda "y" 
    (app 
      (app 
        (lambda "f"
          (var "f"))
        (app 
          (var "f")
          (var "z")))
      (lambda "z" 
        (app 
          (var "z")
          (var "x")))))

complexResult :: Expr 
complexResult = 
  closure "lambda3" "env3" "x"
    (closure "lambda2" "env2" "y"
      (appC 
        (appC 
          (closure "lambda0" "env0" "f"
            (var "f")
            (MkEnv []))
          (appC 
            (envRef "env2" "f")
            (envRef "env2" "z")))
        (closure "lambda1" "env1" "z"
          (appC 
            (var "z")
            (envRef "env1" "x"))
          (MkEnv ["x"])))
      (MkEnv ["f", "z"]))
    (MkEnv [])



closureConvertSpec :: Spec
closureConvertSpec = do
  describe "ClosureConvert.closureConvert" $ do
    it "makes simple closure with no env" $ do
      closureConvert simpleClosure `shouldBe` simpleClosureResult

    it "converts free variable references" $ do
      closureConvert freeVar `shouldBe` freeVarResult

    it "converts nested closures" $ do 
      closureConvert nested `shouldBe` nestedResult

    it "converts applications" $ do 
      closureConvert application `shouldBe` applicationResult

    it "converts complex expressions" $ do 
      closureConvert complex `shouldBe` complexResult
