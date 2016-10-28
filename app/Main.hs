
module Main where

import Lib

import Data.Monoid ((<>))

-- Command-line argument parser
import Options.Applicative

-- Safe String to value conversion
import Safe (readMay)

import System.IO (hSetBuffering, stdout, BufferMode(..))


threadCount :: Parser (Maybe Int)
threadCount = option reader $ long "threads" <> short 't' <> metavar "N"
                                             <> help "Use N threads to download"
                                             <> value Nothing <> showDefault
  where
    reader = str >>= return . readMay

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

