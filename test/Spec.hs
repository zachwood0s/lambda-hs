
import Test.Tasty
import Test.Tasty.Hspec

import ClosureConvertSpec

main :: IO ()
main = do 
  cl <- testSpec "Closure Convert (checked by Hspec)" closureConvertSpec
  defaultMain $ testGroup "Tests" [ cl ]
