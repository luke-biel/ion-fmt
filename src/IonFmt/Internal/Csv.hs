module IonFmt.Internal.Csv where

import IonFmt.Internal.Utils (isCsvLine, cellLength, strip)
import Data.List.Split (splitOn)

-- | Get optimal column content length out of set of rows
csvColumnLengths :: [String] -> [Int]
csvColumnLengths lines = findMaxLengths $ map cellLengths filteredLines
  where
    filteredLines = filter isCsvLine lines
    findMaxLengths (x:xs) = foldl (zipWith max) x xs
    findMaxLengths [] = []

-- | Split ion CSV row into cells
splitCsvLine :: String -> [String]
splitCsvLine line = map strip $ validCells line
  where
    validCells = tail . init . splitCsvLine'
    splitCsvLine' = splitOn "|"

-- | Compute row cell lengths
cellLengths :: String -> [Int]
cellLengths line = map cellLength $ splitCsvLine line
