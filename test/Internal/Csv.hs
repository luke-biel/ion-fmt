module Internal.Csv where

import Test.Hspec
import IonFmt.Internal.Csv

-- splitCsvLine String -> [String]
specSplitCsvLine = do
  it "should split properly formatted CSV with edge delimiters"   $ f "| this | is | CSV |" ["this", "is", "CSV"]
  it "should remove tail and head in CSV with no edge delimiters" $ f "\"this is\" | not | ion | CSV " ["not", "ion"]
  where
    f input expected = splitCsvLine input `shouldBe` expected

-- getCsvLengths [String] -> [Int]
specCsvLengths = do
  it "should compute line lengths excluding trailing and leading whitespaces" $ f
      ["| this | is | CSV |"]
      [4, 2, 3]

  it "should compute line lengths when multiple CSV lines supplied" $ f
      ["| this | is | CSV |", "| with | many many | lines |", "|some with|missing whitespaces|3.14    |"]
      [9, 19, 5]

  it "should skip tail and head when no edge delimiters" $ f
      ["improper | not ion | csv "]
      []

  it "should ignore empty lines" $ f
      ["| some | csv |", "", "| pi | 3.141592 |"]
      [4, 8]

  it "should ignore comments" $ f
      ["| some | csv |", "# and next comes pi", "| pi | 3.141592 |"]
      [4, 8]

  it "should output correctly on longer inputs ascending" $ f
      ["| a |", "| bb |", "| ccc |", "| dddd |"]
      [4]

  it "should output correctly on longer inputs descending" $ f
      ["| aaaa |", "| bbb |", "| cc |", "| d |"]
      [4]
  where
    f input expected = csvColumnLengths input `shouldBe` expected

-- cellLengths :: String -> [Int]
specCellLengths = do
  it "should compute lengths of csv row" $ f
      "| single | csv | line | 3.3 |"
      [6, 3, 4, 3]

  it "should ignore head and taile if not delimited" $ f
      " 3.14 | 1592 | 1.17"
      [4]

  it "should handle leading whitespace" $ f
      "|    \tdata|"
       [4]

  it "should handle trailing whitespace" $ f
      "|data  \t  |"
      [4]
  where
    f input expected = cellLengths input `shouldBe` expected
