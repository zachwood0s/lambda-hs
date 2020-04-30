module Main where

import Parser
import ClosureConvert
import Emit

import Control.Monad.Trans 
import System.Console.Haskeline
import System.IO
import System.Environment
import Data.Text.Lazy.IO as TIO

import LLVM.Pretty (ppllvm)

import qualified LLVM.AST as AST

main :: IO ()
main = runInputT defaultSettings loop 
  where 
    loop = do 
      minput <- getInputLine "ready> "
      case minput of 
        Nothing -> outputStrLn "Goodbye."
        Just input -> do 
          modn <- liftIO $ process input
          case modn of 
            Just modn -> loop 
            Nothing -> loop 


process :: String -> IO (Maybe AST.Module)
process source = do 
  let res = parseToplevel source 
  case res of 
    Left err -> print err >> return Nothing
    Right ex -> do 
      let ast = codegenMod ex 
      TIO.putStrLn (ppllvm ast)
      return $ Just ast

