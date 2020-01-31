module AST where 

type Name = String 

data Expr 
  = Float Double 
  | Integer Integer
  | Abstraction Name Expr 
  | Var String 
  | Application Expr Expr
  | Assignment Name Expr
  deriving (Eq, Ord, Show)

