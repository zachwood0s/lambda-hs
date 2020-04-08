module Main where

import Parser
import ClosureConvert
import Emit
import Codegen

import Control.Monad.Trans 
import System.Console.Haskeline
import System.IO
import System.Environment

import qualified LLVM.AST as AST

main :: IO ()
main = runInputT defaultSettings (loop initModule)
  where 
    loop mod = do 
      minput <- getInputLine "ready> "
      case minput of 
        Nothing -> outputStrLn "Goodbye."
        Just input -> do 
          modn <- liftIO $ process mod input
          case modn of 
            Just modn -> loop modn 
            Nothing -> loop mod

initModule :: AST.Module 
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do 
  let res = parseToplevel source 
  case res of 
    Left err -> print err >> return Nothing
    Right ex -> do 
      ast <- codegenMod modo ex 
      return $ Just ast
