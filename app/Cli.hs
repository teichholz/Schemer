-- | CLI parsing

module Cli where
import RIO
import Options.Applicative
import Types.Types

getCLIInput :: IO Options
getCLIInput = execParser progInfo

progInfo :: ParserInfo Options
progInfo =
  info
    (helper <*> optParser)
    (fullDesc
      <> progDesc "Academic Scheme Parser"
      <> header "Schemer - a simple academic Scheme parser"
    )

optParser :: Parser Options
optParser =
  Options
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )
    <*> strArgument
      (
          metavar "file"
          <> value ""
      )
