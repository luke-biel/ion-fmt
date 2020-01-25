module Main where

import           IonFmt              (formatString)

import           Data.Text           (pack, unpack)
import           Data.Text.IO        as Text (readFile, writeFile)
import           Options.Applicative (Parser, execParser, fullDesc, header,
                                      helper, info, long, metavar, progDesc,
                                      strOption, (<**>))

newtype Args = Args{path :: FilePath}

args :: Parser Args
args = Args
  <$> strOption
      ( long "path"
      <> metavar "TARGET" )

main :: IO ()
main = runMain =<< execParser opts
  where
    opts = info (args <**> helper)
      (  fullDesc
      <> progDesc "Format .ion files"
      <> header "ion autoformatter" )


runMain :: Args -> IO ()
runMain (Args path) = do
  input <- Text.readFile path
  Text.writeFile path (pack . formatString $ unpack input)

