module Parser where 

import Debug.Trace
import Text.Parsec 
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex 
import qualified Text.Parsec.Token as Tok

import Control.Monad (void)
import Data.Functor.Identity

import AST 
import Lexer

expr :: Parser Expr 
expr = Ex.buildExpressionParser table expr'

table = [ [functionOp ]
        ]

functionOp :: Ex.Operator String () Identity Expr 
functionOp = Ex.Infix spacef Ex.AssocLeft

spacef = 
  Tok.whiteSpace lexer
  >> return (\x y -> EApp $ App x y)

expr' :: Parser Expr 
expr' = factor

int :: Parser Expr 
int = (ELit . Int) <$> integer

floating :: Parser Expr 
floating = (ELit . Float) <$> float

variable :: Parser Expr 
variable = (EVar . Var) <$> identifier

abstraction :: Parser Expr 
abstraction = do 
  backslash 
  arg <- identifier 
  dot 
  (EAbs . ALambda) <$> Lambda arg <$> expr

assignment :: Parser Expr 
assignment = do 
  reserved "let"
  Tok.whiteSpace lexer
  name <- identifier
  equals 
  e <- expr 
  return $ EAssign $ Assign name e

factor :: Parser Expr 
factor = try floating 
  <|> int 
  <|> abstraction
  <|> variable
  <|> parens expr

contents :: Parser a -> Parser a 
contents p = do 
  Tok.whiteSpace lexer 
  r <- p 
  eof 
  return r

eol :: Parser ()
eol = void (char '\n') <|> eof

toplevel :: Parser [Expr]
toplevel = many $ do 
  a <- assignment
  semi 
  return a

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"

