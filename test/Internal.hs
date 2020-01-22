module Internal where

import IonFmt.Internal
import Test.Hspec

-- writeWithLengths :: [String] -> [Int] -> String
specWriteWithLengths = do
  it "should expand into valid ion CSV" $ f
    ["this", "is", "csv"]
    [6, 4, 5]
    "| this | is | csv |"

  it "should align everything left" $ f -- TODO: This needs to change to be configurable (or just format numbers right and titles center)
    ["data"]
    [9]
    "| data    |"

  it "should not align properly when too short length" $ f -- TODO: Return Either?
      ["data"]
      [3]
      "| data|"
  where
    f inputStr inputInt expected = writeWithLengths inputStr inputInt `shouldBe` expected
