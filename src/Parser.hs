module Parser where 

import Debug.Trace
import Text.Parsec 
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex 
import qualified Text.Parsec.Token as Tok

import AST 
import Lexer

expr :: Parser Expr 
expr = Ex.buildExpressionParser [[]] application

int :: Parser Expr 
int = Integer <$> integer

floating :: Parser Expr 
floating = Float <$> float

variable :: Parser Expr 
variable = Var <$> identifier

abstraction :: Parser Expr 
abstraction = do 
  backslash 
  arg <- identifier 
  dot 
  Abstraction arg <$> expr

application :: Parser Expr 
application = chainl1 factor $ optional space >> return Application

assignment :: Parser Expr 
assignment = do 
  name <- identifier
  equals 
  Assignment name <$> expr

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

toplevel :: Parser [Expr]
toplevel = sepEndBy assignment (char '\n')

parseExpr :: String -> Either ParseError Expr 
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"