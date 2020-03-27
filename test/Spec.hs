
import Test.Tasty
import Test.Tasty.Hspec

import ClosureConvertSpec
import ParserSpec

main :: IO ()
main = do 
  cl <- testSpec "Closure Convert (checked by Hspec)" closureConvertSpec
  parse <- testSpec "Parser (checked by Hspec)" parserSpec
  defaultMain $ testGroup "Tests" [ cl, parse ]
