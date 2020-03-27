
module ParserSpec
  ( parserSpec 
  ) where

import Test.Tasty
import Test.Tasty.Hspec 
import Text.Parsec

import AST 
import Parser


-------------------------
-- Test Cases 
-------------------------

simpleTopLevel :: String 
simpleTopLevel = "a = \\x. x"

simpleTopLevelResult :: Either ParseError [Expr]
simpleTopLevelResult = 
  Right [EAssign $ Assign "a" 
    (EAbs $ ALambda $ Lambda Nothing "x" (EVar $ Var "x"))]

parserSpec :: Spec 
parserSpec = do 
  describe "Parser.parseToplevel" $ do 
    it "parses a simple top level function" $ do 
      parseToplevel simpleTopLevel `shouldBe` simpleTopLevelResult
