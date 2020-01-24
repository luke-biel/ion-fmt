module IonFmt.Internal where

import           Data.Char             (isNumber)
import           Data.List             (intercalate, isPrefixOf)
import           IonFmt.Internal.Csv   (csvColumnLengths, splitCsvLine,
                                        splitCsvLine')
import           IonFmt.Internal.Utils (isSectionHeader, strip)
import           Text.Read             (readMaybe)

data DataAlignment
  = LeftAlign
  | CenterAlign
  | RightAlign

formatString :: String -> String
formatString s = formatSections $ splitPerSection $ filter (not . null) $ lines s

splitPerSection :: [String] -> [[String]]
splitPerSection = reverse . foldl f []
  where
    f [] x = [[x]]
    f acc@(y:ys) x
      | isSectionHeader x = [x] : acc
      | null x = acc
      | otherwise = (y ++ [x]) : ys

formatSections :: [[String]] -> String
formatSections s = intercalate "\n\n" (map formatSection s) ++ "\n"

formatSection :: [String] -> String
formatSection (header:headers:filler@('|':'-':fs):section) =
  intercalate "\n" $
  header :
  formatHeaders headers lengths : formatFiller lengths : map (`formatCsvLine` lengths) (filter (not . null) section)
  where
    lengths = map (+ 2) $ csvColumnLengths (headers : section)
formatSection (header:section) = intercalate "\n" $ header : map (`formatCsvLine` lengths) (filter (not . null) section)
  where
    lengths = map (+ 2) $ csvColumnLengths section

formatCsvLine :: String -> [Int] -> String
formatCsvLine line@('|':xs) lengths = formatCells (splitCsvLine' line) lengths
formatCsvLine line _                = line

formatFiller :: [Int] -> String
formatFiller lengths = '|' : intercalate "|" (map (`replicate` '-') lengths) ++ "|"

formatHeaders :: String -> [Int] -> String
formatHeaders headers lengths = '|' : (intercalate "|" (formatCells' cells lengths) ++ "|")
  where
    cells = splitCsvLine headers
    formatCells' (s:ss) (i:is) = formatCell CenterAlign s i : formatCells' ss is
    formatCells' [] [] = []

formatCells :: [String] -> [Int] -> String
formatCells cells lengths = '|' : intercalate "|" (formatCells' cells lengths) ++ if length cells == length lengths then "|" else ""
  where
    formatCells' (s:ss) (i:is) = formatCell (discoverAlign s) s i : formatCells' ss is
    formatCells' [s] [] = [formatCell LeftAlign s 0]
    formatCells' [] [] = []

discoverAlign :: String -> DataAlignment
discoverAlign ('%':_) = RightAlign
discoverAlign (x:xs) = if isNumber x then RightAlign else LeftAlign
discoverAlign x = discoverAlign' (readMaybe x :: Maybe Double)
  where
    discoverAlign' (Just _) = RightAlign
    discoverAlign' Nothing  = LeftAlign

formatCell :: DataAlignment -> String -> Int -> String
formatCell _ cell@('#':_) 0 = ' ' : cell
formatCell _ cell 0 = cell
formatCell LeftAlign cell len = ' ' : cell ++ replicate (len - length cell - 1) ' '
formatCell RightAlign cell len = replicate (len - length cell - 1) ' ' ++ cell ++ " "
formatCell CenterAlign cell len = replicate (floor lenBy2) ' ' ++ cell ++ replicate (ceiling lenBy2) ' '
  where
    lenBy2 = fromIntegral (len - length cell) / 2.0
