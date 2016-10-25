
{-# LANGUAGE OverloadedStrings #-}

import Lib

import Test.Tasty
import Test.Tasty.HUnit


import Control.Monad (replicateM, forM_)
import Control.Exception (evaluate)

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map

import qualified Network.Wreq.Session as Wreq


gocardless = "https://gocardless.com/"


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
        [ testGroup "URI path parser unit tests"
            [ testCase "Plain path" . assert $
                maybe False (== URIPath "/api/v3/noun" Map.empty) $
                    maybeParseURIPath "/api/v3/noun"

            , testCase "Plain GET arguments" . assert $
                maybe False (== URIPath "" (Map.fromList [("a", "b"),("c","d")])) $
                    maybeParseURIPath "?a=b&c=d"

            , testCase "Path and GET" . assert $
                maybe False (== URIPath "/api/v3/noun" (Map.fromList [("a", "b"),("c","d")])) $
                    maybeParseURIPath "/api/v3/noun?a=b&c=d"

            , testCase "Empty path" . assert $
                maybe False (== URIPath "" Map.empty) $
                    maybeParseURIPath ""
            ]

        -- Wikipedia specifies an URI's syntax to be of the format:
        -- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
        , testGroup "Domain parser unit tests"
            [ testCase "Plain URL" . assert $
                urlVerifyDomain "example.com" "http://example.com/"

            , testCase "Case sensitivity" . assert $
                urlVerifyDomain "example.com" "https://eXaMpLe.CoM/"

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

        , testGroup "Misc"
            [
            ]
        ]


main :: IO ()
main = do
    flip mapM_ sampleRobotsTxts $ \txt -> do
        print txt
        putChar '\t'
        parseTest robotParser txt
    parseTest uriParser "http://example.com/"

    -- Taggy
    tvars <- Wreq.withAPISession $ \s -> replicateM 100 $ async $ do
        getPage s gocardless
    pages <- mapM readAsync tvars
    print . sum $ sum . fmap length . fmap getPageAssets <$> pages

    -- XML
    --xpages <- Wreq.withAPISession $ replicateM 100 . flip getPageXML gocardless
    --print xpages
    --print $ fmap getPageAssetsXML xpages

    --tests

