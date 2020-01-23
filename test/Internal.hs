module Internal where

import           IonFmt.Internal
import           Test.Hspec

-- formatCell :: DataAlignment -> String -> Int -> String
specFormatCell = do
  it "should align left" $ f
      LeftAlign
      "data"
      10
      " data     "
  it "should align right" $ f
      RightAlign
      "data"
      10
      "     data "
  it "should center" $ f
      CenterAlign
      "data"
      10
      "   data   "
  it "should center when odd length" $ f
      CenterAlign
      "data"
      11
      "    data   "
    where
      f inputAlign inputStr inputInt expected = formatCell inputAlign inputStr inputInt `shouldBe` expected

-- formatCells :: [String] -> [Int] -> String
specFormatCells = do
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
      f inputStr inputInt expected = formatCells inputStr inputInt `shouldBe` expected

-- formatCsvLine :: String -> [Int] -> String
specFormatCsvLine = do
  it "should format csv line according to supplied lengths" $ f
      "| a | b | c |"
      [4, 5, 6]
      "| a  | b   | c    |"

  it "should ignore non-csv lines" $ f
      "# this is comment"
      [5, 6]
      "# this is comment"
    where
      f inputStr inputInt expected = formatCsvLine inputStr inputInt `shouldBe` expected

-- formatSection :: [String] -> String
specFormatSection = do
  it "should format properly empty section" $ f
      ["[CONFIG]"]
      ["[CONFIG]"]

  it "shoudl handle section without breaks" $ f
      [ "[CONFIG]"
      , "|2001-01-01|sunny|80%|"
      , "|2001-01-02|rain|100%|"
      , "|2001-01-03|hurracaine|5%|" ]
      [ "[CONFIG]"
      , "| 2001-01-01 | sunny      | 80%  |"
      , "| 2001-01-02 | rain       | 100% |"
      , "| 2001-01-03 | hurracaine | 5%   |" ]

  it "should handle comments inside csv" $ f
      [ "[CONFIG]"
      , "|2001-01-01|sunny|80%|"
      , "# this is random comment"
      , "|2001-01-02|rain|100%|"
      , "|2001-01-03|hurracaine|5%|" ]
      [ "[CONFIG]"
      , "| 2001-01-01 | sunny      | 80%  |"
      , "# this is random comment"
      , "| 2001-01-02 | rain       | 100% |"
      , "| 2001-01-03 | hurracaine | 5%   |" ]

  it "should handle comments at the end of row" $ f
      [ "[CONFIG]"
      , "|2001-01-01|sunny|80%| # end of line comment"
      , "|2001-01-02|rain|100%|"
      , "|2001-01-03|hurracaine|5%|" ]
      [ "[CONFIG]"
      , "| 2001-01-01 | sunny      | 80%  | # end of line comment"
      , "| 2001-01-02 | rain       | 100% |"
      , "| 2001-01-03 | hurracaine | 5%   |" ]

  it "should handle headers" $ f
      [ "[CONFIG]"
      , "|day|weather|humidity|"
      , "|-|-|-|"
      , "|2001-01-03|hurracaine|5%|" ]
      [ "[CONFIG]"
      , "|     day    |   weather  | humidity |"
      , "|------------|------------|----------|"
      , "| 2001-01-03 | hurracaine | 5%       |" ]
    where
      f input expected = formatSection input `shouldBe` init (unlines expected)

-- splitPerSection :: [String] -> [[String]]
specSplitPerSection = do
  it "should ignore empty sections" $ f
      [ "[EMPTY]" ]
      [
        ["[EMPTY]"]
      ]
  it "should create section" $ f
      [ "[SECTION]"
      , "| some | columns |"
      , "|-|-|"
      , "|value0| value1|" ]
      [
        [ "[SECTION]"
        , "| some | columns |"
        , "|-|-|"
        , "|value0| value1|" ]
      ]
  it "should create multiple sections" $ f
      [ "[ONE]"
      , "| some | columns |"
      , "|-|-|"
      , "|value0| value1|"
      , ""
      , "[TWO]"
      , "| other | columns_1 |"
      , "|--|--|"
      , "|value2| value3|" ]
      [
        [ "[ONE]"
        , "| some | columns |"
        , "|-|-|"
        , "|value0| value1|" ],
        [ "[TWO]"
        , "| other | columns_1 |"
        , "|--|--|"
        , "|value2| value3|" ]
      ]
  it "should keep comments" $ f
      [ "[SECTION]"
      , "| some | columns |"
      , "|-|-|"
      , "# random comment"
      , "|value0| value1|" ]
      [
        [ "[SECTION]"
        , "| some | columns |"
        , "|-|-|"
        , "# random comment"
        , "|value0| value1|" ]
      ]
  it "should handle dictionary sections" $ f
      [ "[SECTION]"
      , "test = true"
      , "test_2 = { dict = \"yea\" }"
      , "# random comment"
      , "test_3 = 3.14" ]
      [
        [ "[SECTION]"
        , "test = true"
        , "test_2 = { dict = \"yea\" }"
        , "# random comment"
        , "test_3 = 3.14" ]
      ]
  where
    f input expected = splitPerSection input `shouldBe` expected
