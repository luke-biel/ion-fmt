module IonFmt.Internal.Csv where

import           Data.List.Split       (splitOn)
import           IonFmt.Internal.Utils (cellLength, isCsvLine, strip)

-- | Get optimal column content length out of set of rows
csvColumnLengths :: [String] -> [Int]
csvColumnLengths lines = findMaxLengths $ map cellLengths filteredLines
  where
    filteredLines = filter isCsvLine lines
    findMaxLengths (x:xs) = foldl (zipWith max) x xs
    findMaxLengths []     = []

-- | Split ion CSV row with trailing column
splitCsvLine' :: String -> [String]
splitCsvLine' line = map strip $ validCells line
  where
    validCells = tail . f
    f = splitOn "|"

-- | Split ion CSV row into cells
splitCsvLine :: String -> [String]
splitCsvLine line = map strip $ validCells line
  where
    validCells = tail . init . f
    f = splitOn "|"

-- | Compute row cell lengths
cellLengths :: String -> [Int]
cellLengths line = map cellLength $ splitCsvLine line
