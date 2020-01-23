module IonFmt.Internal where

import           Data.List             (intercalate, isPrefixOf)
import           IonFmt.Internal.Csv   (csvColumnLengths, splitCsvLine)
import           IonFmt.Internal.Utils (isSectionHeader, strip)

data DataAlignment = LeftAlign | CenterAlign | RightAlign

formatString :: String -> String
formatString s = formatSections $ splitPerSection $ filter (not . null) $ lines s

splitPerSection :: [String] -> [[String]]
splitPerSection = reverse . foldl f []
  where
    f [] x = [[x]]
    f acc@(y:ys) x
      | isSectionHeader x = [x] : acc
      | null x = acc
      | otherwise = ( y ++ [x] ) : ys

formatSections :: [[String]] -> String
formatSections s = intercalate "\n" (map formatSection s)

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
formatCsvLine line@('|':xs) lengths = formatCells (splitCsvLine line) lengths
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
formatCells cells lengths = '|' : (intercalate "|" (formatCells' cells lengths) ++ "|")
  where
    formatCells' (s:ss) (i:is) = formatCell LeftAlign s i : formatCells' ss is
    formatCells' [] []         = []

formatCell :: DataAlignment -> String -> Int -> String
formatCell LeftAlign cell len = ' ' : cell ++ replicate (len - length cell - 1) ' '
formatCell RightAlign cell len = replicate (len - length cell - 1) ' ' ++ cell ++ " "
formatCell CenterAlign cell len = replicate (ceiling lenBy2) ' ' ++ cell ++ replicate (floor lenBy2) ' '
  where
    lenBy2 = fromIntegral (len - length cell) / 2.0
