module Main where

import Parser
import ClosureConvert

import Control.Monad.Trans 
import System.Console.Haskeline

main :: IO ()
main = print test{-runInputT defaultSettings loop
  where 
    loop = do 
      minput <- getInputLine "ready> "
      case minput of 
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
	-}

process :: String -> IO ()
process line = do 
  let res = parseToplevel line 
  case res of 
    Left err -> print err 
    Right ex -> print ex
