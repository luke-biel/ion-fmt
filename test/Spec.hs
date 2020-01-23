import           Internal
import           Internal.Csv
import           Internal.Utils
import           Test.Hspec

main :: IO ()
main =
  hspec $
  describe "IonFmt" $
    describe "Internal" $ do
      context "#formatCells"       specFormatCells
      context "#formatCsvLine"     specFormatCsvLine
      context "#formatSection"     specFormatSection
      context "#formatCell"        specFormatCell
      context "#splitPerSection"   specSplitPerSection

      describe "Utils" $ do
        context "#isSectionHeader" specIsSectionHeader
        context "#isCsvLine"       specIsCsvLine
        context "#strip"           specStrip
        context "#cellLength"      specCellLength

      describe "Csv" $ do
        context "#splitCsvLine"    specSplitCsvLine
        context "#csvLengths"      specCsvLengths
        context "#cellLengths"     specCellLengths
