module Toplevel where 

import LLVM.AST
import LLVM.Pretty

import Data.Text.Lazy (unpack)

import System.IO
import System.Directory
import System.Process
import System.Posix.Temp

import Control.Exception (bracket)

compile :: Module -> FilePath -> IO ()
compile llvmModule outfile = 
  bracket (mkdtemp "build") removePathForcibly $ \buildDir -> 
    withCurrentDirectory buildDir $ do 
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      let runtime = "../src/runtime.c"

      hPutStrLn llvmHandle (unpack $ ppllvm llvmModule)
      hClose llvmHandle

      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

run :: Module -> IO String 
run llvmModule = do 
  compile llvmModule "./a.out"
  result <- readProcess "./a.out" [] []
  removePathForcibly "./a.out"
  return result
