module Lib where

import Data.List (isPrefixOf, intercalate, dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Char (isSpace)

formatString :: String -> String
formatString s = formatSections $ splitPerSection $ filter (not . null) $ lines s

splitPerSection :: [String] -> [[String]]
splitPerSection = foldr f []
  where
    f x [] = [[x]]
    f x (y:ys)
      | isSectionHeader x = (x:y):ys
      | otherwise = [x]:y:ys

isSectionHeader :: String -> Bool
isSectionHeader ('[':_) = True
isSectionHeader _ = False

formatSections :: [[String]] -> String
formatSections s = intercalate "\n" (map formatSection s)

formatSection :: [String] -> String
formatSection (header:section) = intercalate "\n" $ header : map formatCsvLine section
  where
    formatCsvLine line@('|':xs) = writeWithLengths (splitCsvLine line) lengths
    formatCsvLine line = line
    lengths = map (+ 2) $ getCsvLengths section

writeWithLengths :: [String] -> [Int] -> String
writeWithLengths s i = "|" ++ intercalate "|" (expandedCells s i) ++ "|"
  where
    expandedCells (s:ss) (i:is) = expandedCell s i : expandedCells ss is
    expandedCells [] [] = []
    expandedCell s i = ' ' : s ++ replicate (i - length s - 1) ' '

splitCsvLine :: String -> [String]
splitCsvLine line = map strip $ validCells line
  where
    validCells = tail . init . splitCsvLine'
    splitCsvLine' = splitOn "|"

getCsvLengths :: [String] -> [Int]
getCsvLengths lines = optimizeCellLengths $ map getCellLengths lines
  where
    countLength cell = length $ strip cell
    getCellLengths line = map countLength (splitCsvLine line)
    optimizeCellLengths (x:xs) = foldl chooseMax x xs
    chooseMax (x:xs) (y:ys) = max x y : chooseMax xs ys
    chooseMax [] [] = []

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
