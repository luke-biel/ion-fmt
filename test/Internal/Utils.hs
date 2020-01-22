module Internal.Utils where

import           IonFmt.Internal.Utils
import           Test.Hspec

-- isSectionHeader :: String -> Bool
specIsSectionHeader = do
  it "should properly recognize ion section headers" $ f  "[CONTRACT]"
  it "should properly recognize ion lines"           $ f' "| \"text\" | 3.14 |"
  it "should properly recognize whitespace lines"    $ f' "\t\t   \t"
  it "should properly recognize empty lines"         $ f' ""
  it "should properly recognize comment lines"       $ f' "# This is a comment"
  where
    f str = str `shouldSatisfy` isSectionHeader
    f' str = str `shouldNotSatisfy` isSectionHeader

-- strip :: String -> String
specStrip = do
  it "should not strip correct strings" $ f "correct" "correct"
  it "should not strip empty string"    $ f "" ""
  it "should strip whitespace string"   $ f "   \t" ""
  it "should strip incorrect string"    $ f " incorrect \t" "incorrect"
  where
    f input expected = strip input `shouldBe` expected

-- isCsvLine :: String -> Bool
specIsCsvLine = do
  it "should correctly recognize ion CSV lines"                    $ f  "| this | is | CSV |"
  it "should correctly mark comments as invalid"                   $ f' "# this is comment"
  it "should correctly mark empty lines as invalid"                $ f' ""
  it "should correctly mark csv without open delimiter as invalid" $ f' "this | is | sparta"
  where
    f str = str `shouldSatisfy` isCsvLine
    f' str = str `shouldNotSatisfy` isCsvLine

-- cellLength :: String -> Int
specCellLength = do
  it "should compute string length"    $ f "this is long string" 19
  it "should skip leading whitespace"  $ f "  \tleading"         7
  it "should skip trailing whitespace" $ f "trailing\t \t"       8
  where
    f input expected = cellLength input `shouldBe` expected
