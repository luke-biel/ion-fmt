module IonFmt.Internal.Utils where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn, startsWith)

-- | Determine if input string is ion section header `[HEADER]`
isSectionHeader :: String -> Bool
isSectionHeader ('[':_) = True
isSectionHeader _ = False

-- | Determine if selected String is ion CSV row
isCsvLine :: String -> Bool
isCsvLine ('|':_) = True
isCsvLine _ = False

-- | Strip string from trailing and leading whitespace
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- | Compute cell length
cellLength :: String -> Int
cellLength cell = length $ strip cell
