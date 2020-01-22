module IonFmt.Internal where

import Data.List (isPrefixOf, intercalate)
import IonFmt.Internal.Utils (isSectionHeader, strip)
import IonFmt.Internal.Csv (splitCsvLine, csvColumnLengths)

formatString :: String -> String
formatString s = formatSections $ splitPerSection $ filter (not . null) $ lines s

splitPerSection :: [String] -> [[String]]
splitPerSection = foldr f []
  where
    f x [] = [[x]]
    f x (y:ys)
      | isSectionHeader x = [x]:y:ys
      | otherwise = (x:y):ys

formatSections :: [[String]] -> String
formatSections s = intercalate "\n" (map formatSection s)

formatSection :: [String] -> String
formatSection (header:section) = intercalate "\n" $ header : map formatCsvLine section
  where
    formatCsvLine line@('|':xs) = writeWithLengths (splitCsvLine line) lengths
    formatCsvLine line = line
    lengths = map (+ 2) $ csvColumnLengths section

writeWithLengths :: [String] -> [Int] -> String
writeWithLengths s i = "|" ++ intercalate "|" (expandedCells s i) ++ "|"
  where
    expandedCells (s:ss) (i:is) = expandedCell s i : expandedCells ss is
    expandedCells [] [] = []
    expandedCell s i = ' ' : s ++ replicate (i - length s - 1) ' '

