module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok 

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style 
  where 
    ops = ["."] 
    names = ["let", "in"]
    style = emptyDef 
      { Tok.commentLine = "--"
      , Tok.reservedOpNames = ops
      , Tok.reservedNames = names
      }

integer :: Parser Integer 
integer = Tok.integer lexer 

float :: Parser Double 
float = Tok.float lexer

parens :: Parser a -> Parser a 
parens = Tok.parens lexer 

identifier :: Parser String 
identifier = Tok.identifier lexer 

reserved :: String -> Parser ()
reserved = Tok.reserved lexer 

reservedOp :: String -> Parser () 
reservedOp = Tok.reservedOp lexer

backslash :: Parser String
backslash = Tok.symbol lexer "\\" 

equals :: Parser String 
equals = Tok.symbol lexer "="

dot :: Parser String
dot = Tok.dot lexer