
{-# LANGUAGE OverloadedStrings, TypeApplications, PartialTypeSignatures,
             TupleSections, BangPatterns
#-}

module Lib where


-- {{{ Imports
-- Helpful control structure function for failure/choice
import Control.Applicative ((<|>))
-- Concurrency
import Control.Concurrent (forkIO)
-- Mutable shared state
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
-- Helpful control structure functions for dealing with nested data
import Control.Lens
-- Helpful control structure functions
import Control.Monad (forM_, void, forever)

-- Efficient textual parser
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Combinator as Atto
-- Efficient, low-level String
import qualified Data.ByteString as BS
-- Lazily evaluated ByteString
import qualified Data.ByteString.Lazy as BL
-- Case insensitive text
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
-- List utilities
import qualified Data.List as List
-- Key/value map data structure
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- Safe failures
import Data.Maybe (catMaybes)
-- Efficient UTF8 String
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8With)
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Data.Text.Lazy.Encoding as LazyText (decodeUtf8With)
-- Vector: efficient arrays
import Data.Vector (Vector)
import qualified Data.Vector as Vec

-- Wreq is for HTTP requests
import qualified Network.Wreq.Session as Wreq
import qualified Network.Wreq as Wreq (responseBody, defaults, header)

-- Safe(r) paths; we use this for URL construction
import System.FilePath.Posix ((</>))
-- "Unsafe" IO functions; only used safely here
import System.IO.Unsafe (unsafePerformIO)

-- XML parser
import qualified Text.Taggy as Taggy hiding (htmlWith)
import qualified Text.Taggy.Lens as Taggy
-- }}}


-- The Blackbird compose function
-- f `blackbird` g where g takes two arguments instead of one.
blackbird = ( . ) . ( . )


-- Simple data structure for Robots.txt rules
data RobotRules = RobotUserAgent Text
                | RobotDisallowed Text
                | RobotIgnored
                deriving (Show, Eq)

robotParser = do
    Atto.many' $ Atto.choice [ userAgent
                             , disallowed
                             , comment
                             ]
  where
    userAgent = spaceAround $ do
        Atto.asciiCI "User-Agent:"
        Atto.skipWhile (== ' ')
        RobotUserAgent <$> (Atto.takeWhile1 (/= '\n') <|> pure "")
    disallowed = spaceAround $ do
        Atto.asciiCI "Disallow:"
        Atto.skipWhile (== ' ')
        RobotDisallowed <$> (Atto.takeWhile1 (/= '\n') <|> pure "")
    comment = spaceAround $ do
        Atto.char '#'
        !comment <- Atto.takeWhile1 (/= '\n') <|> pure ""
        return RobotIgnored
    whiteSpace = Atto.skipWhile (`elem` [' ', '\n']) <|> pure ()
    surround p0 p1 = p0 *> p1 <* p0
    spaceAround = surround whiteSpace

ezParser = map toRobotData
         -- Remove empty lines
         . filter (/= "")
         -- Remove comment lines
         . filter (not . Text.isPrefixOf "#")
         -- Split on lines
         . Text.lines
         -- Remove spaces
         . Text.filter (/= ' ')
  where
    toRobotData x = case () of
      _ | isPrefixOfCIText "User-Agent:" x ->
            RobotUserAgent $ Text.drop 11 x

        | isPrefixOfCIText "Disallow:" x ->
            RobotDisallowed $ Text.drop 9 x

        | otherwise -> RobotIgnored

    isPrefixOfCIText x y = CI.mk (Text.take (Text.length x) y) == CI.mk x

-- | Parser to obtain domain given an URL
domainParser :: Atto.Parser (CI Text)
domainParser = do
    -- Protocol
    Atto.choice [ Atto.string "http:"
                , Atto.string "https:"
                -- Consume nothing and continue
                , pure ""
                ]
    -- Separator
    Atto.string "//"
    -- User/password
    Atto.choice [ Atto.takeWhile1 uriUserInfo >> Atto.char '@'
                -- Consume nothing and continue
                , pure ' '
                ]
    -- Domain
    domain <- CI.mk <$> Atto.takeWhile1 uriRegName
    -- Port
    Atto.choice [ Atto.char ':' >> Atto.takeWhile1 isDigit
                , pure ""
                ]
    -- Path
    Atto.choice [ Atto.char '/' >> Atto.takeText >> Atto.endOfInput
                , Atto.char '/' >> Atto.endOfInput
                , Atto.endOfInput
                ]

    return domain
  where
    isDigit c = c >= '0' && c <= '9'
    isAlpha c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
    -- Allowed characters in a URI's reg-name ABNF
    uriRegName :: Char -> Bool
    uriRegName c = or [ isAlpha c
                      , isDigit c
                      , elem c ("-._~%!$&'()*+,;=" :: String)
                      ]
    -- Allowed characters in a URI's userinfo ABNF
    uriUserInfo :: Char -> Bool
    uriUserInfo c = uriRegName c || c == ':'

-- | Verify that a URL is of a specific domain
urlVerifyDomain :: Text -> Text -> Bool
urlVerifyDomain domain url = maybe False (CI.mk domain ==)
                           $ maybeParseDomain url

-- | Parse a domain from an URL into a Maybe
maybeParseDomain :: Text -> Maybe (CI Text)
maybeParseDomain url = Atto.maybeResult
                     $ Atto.parse domainParser url `Atto.feed` ""


-- | Download and parse HTML into contained hyperlinks
getPageLinks :: Wreq.Session -> URL -> IO [Text]
getPageLinks session url = do
    request <- Wreq.getWith opts session url

    let body :: LazyText.Text
                        -- Get the response body
        body = request ^. Wreq.responseBody
                        -- Lenient UTF-8 decoding to Text
                        . to (LazyText.decodeUtf8With Text.lenientDecode)

                -- Remove those without href attributes
    let links = catMaybes $ do
            -- Find all <a> tags with href attributes
            body ^.. Taggy.htmlWith False . Taggy.allNamed (only "a")
                   . Taggy.attr "href"

    return links

-- Make the crawler identifiable with a unique User-Agent
opts = Wreq.defaults
     & set (Wreq.header "User-Agent") ["milk-biscuit-teacake"]

-- TODO
-- | Download and return robots.txt disallows
getRobotRules :: Wreq.Session -> URL -> IO [RobotRules]
getRobotRules session host = do
    let url = host </> "robots.txt"
    print url
    request <- Wreq.getWith opts session url

    let body :: BL.ByteString
        body = view Wreq.responseBody request

        textBody :: Text
        textBody = Text.decodeUtf8With Text.lenientDecode (BL.toStrict body)

    let rules = filter (/= RobotIgnored)
              . maybe [] id . Atto.maybeResult
              $ Atto.parse robotParser textBody `Atto.feed` ""

    return rules

linkCache :: TVar (Map Text Int)
linkCache = unsafePerformIO $ newTVarIO Map.empty

linkQueue :: TQueue String
linkQueue = unsafePerformIO $ newTQueueIO

-- TODO
crawl :: Wreq.Session -> URL -> IO ()
crawl session url = do
    robotRules <- getRobotRules session url

    print robotRules

    let disallowedPaths = do
            -- Filter non-RobotDisallowed
            RobotDisallowed x <- robotRules
            return x

    links <- getPageLinks session url

    print links

    let allowedLinks = List.deleteFirstsBy
                       (not `blackbird` Text.isPrefixOf)
                       disallowedPaths
                       links

    print allowedLinks

    visitedLinks <- Map.keys <$> readTVarIO linkCache

    -- Filter visited links
    let unvisitedLinks = List.deleteFirstsBy (/=) visitedLinks allowedLinks
        unvisitedMap = Map.fromList $ map (,1) unvisitedLinks

    print unvisitedLinks

    atomically $ modifyTVar' linkCache $ \cacheMap ->
        Map.unionWith (+) cacheMap unvisitedMap

    atomically $ mapM_ (writeTQueue linkQueue . Text.unpack) unvisitedLinks

    return ()


-- TODO concurrently re-use sessions!
--      - How many?
--          - As many as there are scraped links
--              - max (length links) (length sessions)
--          - Maybe there's a website limit
--      - Can we store sessions for re-use?
--      - Use `linkQueue` with `sessionHandler`, when one finishes
--        just keep reading the queue forever until exhausted.
--          - How do we spread the workload evenly across GET requests?
--              - Configurable command line argument?

sessionPool :: TVar (Vector Wreq.Session)
sessionPool = unsafePerformIO $ newTVarIO Vec.empty

type URL = String

-- | 
sessionHandler :: IO ()
                 -- No-cookie HTTP sessions
sessionHandler = Wreq.withAPISession $ \session -> do
    forever $ do
        url <- atomically $ readTQueue linkQueue
        print url
        crawl session url
    atomically $ putTMVar finishedState () -- FIXME this is just a trick


finishedState = unsafePerformIO $ newEmptyTMVarIO

-- | Initialise the crawler with concurrent sessionHandlers
initCrawler :: URL -> IO ()
initCrawler url = do
    let mdom = maybeParseDomain $ Text.pack url

    -- Get initial, homepage links
    Wreq.withAPISession (`crawl` url)

    -- Run sessionHandlers in separate threads
    forM_ [1 .. 6] $ const (forkIO sessionHandler)

    -- Block until we're finished
    atomically $ takeTMVar finishedState

initProgram :: [URL] -> IO ()
initProgram urls = do
    -- For each website
    forM_ urls initCrawler

