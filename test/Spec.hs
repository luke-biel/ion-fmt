import Internal.Utils
import Internal.Csv
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "IonFmt.Internal.Utils" $ do
    context "#isSectionHeader" specIsSectionHeader
    context "#isCsvLine" specIsCsvLine
    context "#strip" specStrip
    context "#cellLength" specCellLength
  describe "IonFmt.Internal.Csv" $ do
    context "#splitCsvLine" specSplitCsvLine
    context "#csvLengths" specCsvLengths
    context "#cellLengths" specCellLengths
