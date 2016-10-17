
module Main where

import Lib

-- Command-line argument parser
import Options.Applicative

import System.IO (hSetBuffering, stdout, BufferMode(..))


threadCount :: Parser Int
threadCount = option auto $ long "threads" <> short 't' <> metavar "N"
                                           <> help "Use N threads to download"
                                           <> value 6 <> showDefault

argsParser :: Parser Args
argsParser = Args
    <$> threadCount
    <*> some (argument str $ metavar "URLS...")

parserOpts :: ParserInfo Args
parserOpts = info (helper <*> argsParser) $
                  fullDesc <> progDesc "Crawl website URLs and output static assets"
                           <> header "crawler - a concurrent website crawler"


main :: IO ()
main = do
    -- Flush stdout after newlines
    hSetBuffering stdout NoBuffering
    -- Run program with specified command-line arguments
    execParser parserOpts >>= initProgram

