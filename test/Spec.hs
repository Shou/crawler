
{-# LANGUAGE OverloadedStrings #-}

import Lib

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Attoparsec.Text as Atto


parseFeed p t = Atto.parse p t `Atto.feed` ""
parseTest p = print . parseFeed p
parseMaybe p = Atto.maybeResult . parseFeed p


sampleRobotsTxts =
    [ mconcat [ "\n# Comment\nUser-Agent: *\n"
              , "\nDisallow: /secret/\nDisallow:\nDisallow: /a/\n"
              ]
    , mconcat [ "\n# Comment\nUser-Agent: *\nDisallow: /a/\n" ]
    , mconcat [ "\n#" ]
    , mconcat [ "" ]
    ]

tests = defaultMain $ do
    testGroup "Tests"
        [ testGroup "Empty"
            [
            ]

        -- Wikipedia specifies an URI's syntax to be of the format:
        -- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
        , testGroup "Domain parser unit tests"
            [ testCase "Plain URL" . assert $
                urlVerifyDomain "example.com" "http://example.com/"

            , testCase "HTTPS" . assert $
                urlVerifyDomain "example.com" "https://example.com/"

            , testCase "Relative protocol" . assert $
                urlVerifyDomain "example.com" "//example.com/"

            , testCase "Subdomain" . assert $
                urlVerifyDomain "sub.example.com" "http://sub.example.com/"

            , testCase "User credentials" . assert $
                urlVerifyDomain "example.com" "http://user:pass@example.com/"

            , testCase "Port number" . assert $
                urlVerifyDomain "example.com" "http://example.com:8000/"

            , testCase "Trailing paths" . assert $
                urlVerifyDomain "example.com" "http://example.com/file.html#id"

            , testCase "Trailing @" . assert $
                urlVerifyDomain "example.com" "http://example.com/@"

            , testCase "Everything" . assert $
                urlVerifyDomain "sub.example.com" "https://user:pass@sub.example.com:8000/file.html#id@"

            , testCase "Invalid URL" . assert $
                not $ urlVerifyDomain "example.com" "http://ex\"ample.com/"
            ]
        ]


main :: IO ()
main = do
    flip mapM_ sampleRobotsTxts $ \txt -> do
        print txt
        putChar '\t'
        parseTest robotParser txt
        putChar '\t'
        print $ ezParser txt
    parseTest domainParser "http://example.com/"

    tests

