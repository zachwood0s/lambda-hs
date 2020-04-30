module Main where

import Options.Applicative
import LLVM.Pretty

import Text.Pretty.Simple
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Lazy.IO as TIO (putStrLn)

import Parser
import Emit
import Toplevel

data Action = Ast | LLVM | Compile FilePath | Run
data Options = Options { action :: Action, infile :: FilePath}

actionP :: Parser Action
actionP = 
  flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' LLVM 
        (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag' Compile
        (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption (short 'o' <> value "a.out" <> metavar "FILE")
    <|> pure Run

optionsP :: Parser Options
optionsP = 
  Options 
    <$> actionP 
    <*> strArgument (help "Source file" <> metavar "FILE")

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
  where 
    withInfo opts desc = info (helper <*> opts) $ progDesc desc 
    infoString 
      = "Run the lambda compiler on the given file.\
      \Passing no flags will compile the file, execute it, and print the output."

runOpts :: Options -> IO ()
runOpts (Options action infile) = do
  program <- readFile infile 
  let parseTree = parseToplevel program
  case parseTree of 
    Left err -> print err 
    Right ast -> case action of 
      Ast -> print ast
      _ -> 
        let llvm = codegenMod ast 
        in case action of 
          LLVM -> TIO.putStrLn (ppllvm llvm)
          Compile outfile -> compile llvm outfile 
          Run -> run llvm >>= putStr 
          Ast -> error "unreachable"
          
