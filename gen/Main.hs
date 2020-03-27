module Main where 

import Data.Generics.Alloy.GenInstances
import AST

main = writeInstancesTo ( allInstances GenWithoutOverlapped)
       [ genInstance ( undefined :: Expr ) ]
       ([ "module ASTInstances where"
        , "import qualified AST"
        , "import qualified GHC.Maybe"
        ] ++ instanceImports )
       "src/ASTInstances.hs"
