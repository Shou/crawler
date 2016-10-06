
{-# LANGUAGE OverloadedStrings, TypeApplications, PartialTypeSignatures,
             TupleSections, BangPatterns
#-}

module Lib where


-- {{{ Imports
-- Helpful control structure function for failure/choice
import Control.Applicative ((<|>))
-- Concurrency
import Control.Concurrent (forkIO, myThreadId, ThreadId)
-- Mutable shared state
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
-- Helpful control structure functions for dealing with nested data
import Control.Lens
-- Helpful control structure functions
import Control.Monad (forM_, void, forever, when, guard, unless)
import qualified Control.Monad.STM as STM (retry)

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
-- Function utilities
import Data.Function (on)
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
import qualified Network.Wreq as Wreq (responseBody, defaults, header)
import qualified Network.Wreq.Session as Wreq
import qualified Network.Wreq.Types as Wreq

-- Safe(r) paths; we use this for URL construction
import System.FilePath.Posix ((</>), dropDrive)
-- "Unsafe" IO functions; only used safely here
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)

-- XML parser
import qualified Text.Taggy as Taggy hiding (htmlWith)
import qualified Text.Taggy.Lens as Taggy
-- }}}


-- URLs are just Strings
type URL = String

-- | Data structure for Robots.txt rules
data RobotRules = RobotUserAgent Text
                | RobotDisallowed Text
                | RobotIgnored
                deriving (Show, Eq)

-- | URI data structure
data URI = URI { uriProtocol :: Text
               , uriUserinfo :: Text
               , uriDomain :: Text
               , uriPort :: Text
               , uriPath :: Text
               }
               deriving (Show)

-- | Crawler config data structure
data Config = Config { confDisPaths :: [Text]
                     , confDomain :: Text
                     }


logger = unsafePerformIO newTQueueIO
logPrint a = mapM_ ($ a) [atomically . writeTQueue logger . show]


-- | Visited link cache and frequency
linkCache :: TVar (Map Text Int)
linkCache = unsafePerformIO $ newTVarIO $ Map.fromList [("/",1),("",1)]

-- | Unvisited link queue
linkQueue :: TQueue Text
linkQueue = unsafePerformIO newTQueueIO

threadDoneCache :: TVar (Vector ThreadId)
threadDoneCache = unsafePerformIO $ newTVarIO Vec.empty


-- | Parser for robots.txt files
robotParser :: Atto.Parser [RobotRules]
robotParser = do
    -- Parse a list of user agents, disallowed paths, and ignored content
    Atto.many' $ Atto.choice [ userAgent
                             , disallowed
                             , comment
                             ]
  where
    userAgent = spaceAround $ do
        -- Case-insensitive match
        Atto.asciiCI "User-Agent:"
        -- Ignore following spaces
        Atto.skipWhile (== ' ')
        -- Take until newline and return the user agent
        RobotUserAgent <$> (Atto.takeWhile1 (/= '\n') <|> pure "")
    disallowed = spaceAround $ do
        -- Case-insensitive match
        Atto.asciiCI "Disallow:"
        -- Ignore following spaces
        Atto.skipWhile (== ' ')
        -- Take until newline and return the disallowed path
        RobotDisallowed <$> (Atto.takeWhile1 (/= '\n') <|> pure "")
    comment = spaceAround $ do
        -- Comments start with hashes
        Atto.char '#'
        -- Take until a newline is reached
        !comment <- Atto.takeWhile1 (/= '\n') <|> pure ""
        -- Just return ignored content constructor
        return RobotIgnored
    -- Skip many spaces or newlines
    whiteSpace = Atto.skipWhile (`elem` [' ', '\n']) <|> pure ()
    -- Combinator: one parser surrounded by another parser
    surround p0 p1 = p0 *> p1 <* p0
    -- A parser surrounded by whitespace
    spaceAround = surround whiteSpace

-- | Parser to obtain domain given an URL
uriParser :: Atto.Parser URI
uriParser = URI <$> protocol <* separator <*> userinfo <*> domain
                   <*> port <*> path
  where
    protocol = do
        Atto.choice [ Atto.string "http:"
                    , Atto.string "https:"
                    -- Consume nothing and continue
                    , pure ""
                    ]
    separator = Atto.string "//"
    userinfo = do
        Atto.choice [ Atto.takeWhile1 isUserInfo <* Atto.char '@'
                    -- Consume nothing and continue
                    , pure ""
                    ]
    domain = Atto.takeWhile1 isRegName
    port = do
        Atto.choice [ Atto.char ':' >> Atto.takeWhile1 isDigit
                    , pure ""
                    ]
    path = do
        Atto.choice [ Atto.char '/' >> Atto.takeWhile1 (/= '#')
                    , Atto.string "/"
                    , pure ""
                    ]
    -- Helpers
    isDigit c = c >= '0' && c <= '9'
    isAlpha c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
    -- Allowed characters in a URI's reg-name ABNF
    isRegName :: Char -> Bool
    isRegName c = or [ isAlpha c
                     , isDigit c
                     , elem c ("-._~%!$&'()*+,;=" :: String)
                     ]
    -- Allowed characters in a URI's userinfo ABNF
    isUserInfo :: Char -> Bool
    isUserInfo c = isRegName c || c == ':'

-- | Parse a domain from an URL into a Maybe
maybeParseURI :: Text -> Maybe URI
maybeParseURI url = Atto.maybeResult
                  $ Atto.parse uriParser url `Atto.feed` ""

-- | Verify that a URI contains the specified domain
uriVerifyDomain :: Text -> URI -> Bool
uriVerifyDomain domain uri = on (==) CI.mk domain $ uriDomain uri

-- | Verify that a URL is of a specific domain
urlVerifyDomain :: Text -> Text -> Bool
urlVerifyDomain domain = maybe False id
                       . fmap (uriVerifyDomain domain)
                       . maybeParseURI

-- | Download a webpage and return the parsed contents
getPage :: Wreq.Session -> URL -> IO (Maybe Taggy.Node)
getPage session url = do
    request <- Wreq.getWith opts session url

    let body :: LazyText.Text
                        -- Get the response body
        body = request ^. Wreq.responseBody
                        -- Lenient UTF-8 decoding to Text
                        . to (LazyText.decodeUtf8With Text.lenientDecode)

    return $ body ^? Taggy.htmlWith False


-- | Parse HTML into contained hyperlinks
getPageLinks :: Taggy.Node -> IO [Text]
getPageLinks node = do
                -- Remove those without href attributes
    let links = catMaybes $ do
            -- Find all <a> tags with href attributes
            node ^.. Taggy.allNamed (only "a") . Taggy.attr "href"

    return links

-- | Parse HTML into contained asset links
getPageAssets :: Taggy.Node -> IO [Text]
getPageAssets node = do
    let imgs = catMaybes $ do
            node ^.. Taggy.allNamed (only "img") . Taggy.attr "src"
        scripts = catMaybes $ do
            node ^.. Taggy.allNamed (only "script") . Taggy.attr "src"
        styles = catMaybes $ do
            node ^.. Taggy.allNamed (only "link")
                   . Taggy.attributed (ix "rel" . only "text/css")
                   . Taggy.attr "href"

    return undefined


noExceptions _ _ _ = Nothing

    -- Don't throw exceptions on HTTP codes
opts = Wreq.defaults { Wreq.checkStatus = Just noExceptions }
    -- Make the crawler identifiable with a unique User-Agent
     & set (Wreq.header "User-Agent") ["milk-biscuit-teacake"]

-- | Download and return robots.txt disallows
getRobotRules :: Wreq.Session -> URL -> IO [RobotRules]
getRobotRules session host = do
    let url = host </> "robots.txt"
    request <- Wreq.getWith opts session url

    let body :: BL.ByteString
        body = view Wreq.responseBody request

        textBody :: Text
        textBody = Text.decodeUtf8With Text.lenientDecode (BL.toStrict body)

              -- Filter ignored lines
    let rules = filter (/= RobotIgnored)
              -- Default to empty list on failure
              . maybe [] id . Atto.maybeResult
              -- Parse the robots.txt file
              $ Atto.parse robotParser textBody `Atto.feed` ""

    return rules

crawl :: Wreq.Session -> URL -> Config -> IO ()
crawl session url config = do
    let disallowedPaths = confDisPaths config
        domain = confDomain config

    -- Download page
    mnode <- getPage session url

    -- Get page links; return empty list on getPage parse failure
    links <- maybe (pure []) getPageLinks mnode

    -- Get page assets; return empty list on getPage parse failure
    assets <- maybe (pure []) getPageAssets mnode

    -- Get visited links from the cache
    visitedLinks <- Map.keys <$> readTVarIO linkCache

    -- Remove duplicates
    let uniqueLinks = List.nub links

    -- Sanitised paths
    let paths = do
            -- For each link
            link <- uniqueLinks
            -- Parse link to Maybe URI
            let muri = maybeParseURI link
            -- Verify domain; assume no domain on parse failure
            guard $ maybe True id $ uriVerifyDomain domain <$> muri
            -- Remove fragment from link
            let noFragmentLink = Text.takeWhile (/= '#') link
            -- Use link as path instead of uriPath on parse failure
            let path = maybe noFragmentLink uriPath muri
            -- No disallowed paths
            guard . not $ any (`Text.isPrefixOf` path) disallowedPaths
            -- No visited paths
            guard . not $ any (== path) visitedLinks
            return path

    let pathsMap = Map.fromList $ map (,1) paths

    tid <- myThreadId

    atomically $ do
        -- Count link frequency
        modifyTVar' linkCache $ flip (Map.unionWith (+)) pathsMap

        -- Queue unvisited links
        mapM_ (writeTQueue linkQueue) paths

        -- No unvisited links; thread finished?
        modifyTVar' threadDoneCache (Vec.cons tid)


-- | Re-use HTTP session to crawl
sessionHandler :: URL -> Config -> IO ()
sessionHandler homepage config = do
    -- No-cookie HTTP sessions
    Wreq.withAPISession $ \session -> forever $ do
        -- Read next link
        path <- atomically $ Text.unpack <$> readTQueue linkQueue
        -- Thread unfinished; remove done status
        tid <- myThreadId
        atomically $ modifyTVar' threadDoneCache (Vec.filter (/= tid))
        -- Safely combine homepage and path to a URL
        let url = homepage </> dropDrive path
        logPrint url
        -- Crawl page for more links
        crawl session url config

-- | Initialise the crawler with concurrent sessionHandlers
initCrawler :: URL -> IO ()
initCrawler url = do
    let domain = maybe "" uriDomain . maybeParseURI $ Text.pack url

    -- Get initial homepage links, and robots.txt disallowed paths
    config <- Wreq.withAPISession $ \session -> do
        robotRules <- getRobotRules session url

        let !disallowedPaths = do
                -- Filter non-RobotDisallowed
                RobotDisallowed (!x) <- robotRules
                return x

        let config = Config disallowedPaths domain

        crawl session url config

        return config

    -- Run sessionHandlers in separate threads
    forM_ [1 .. 6] $ const (forkIO $ sessionHandler url config)

    dones <- atomically $ do
        dones <- readTVar threadDoneCache
        -- All threads are currently done, we may be finished
        if Vec.length dones == 6 then do
            isEmptyLinkQueue <- isEmptyTQueue linkQueue
            -- When the link queue is empty, we're done; otherwise retry
            unless isEmptyLinkQueue STM.retry

        -- Only some threads are done; retry
        else STM.retry

        return dones

    putStrLn "Finished."
    putChar '\t'
    putStr $ "Dones: " ++ show (Vec.length dones)
    putChar ' ' >> putStrLn (show dones)
    linkMap <- readTVarIO linkCache
    putChar '\t'
    putStrLn $ "URLs crawled: " ++ show (Map.size linkMap - 1)
    putChar '\t'
    putStrLn $ "Total links: " ++ show (sum $ Map.elems linkMap)
    putChar '\t'
    putStrLn $ "Total links length: " ++ show (Text.length . mconcat $ Map.keys linkMap)

    -- Exit program
    exitSuccess


initProgram :: [URL] -> IO ()
initProgram urls = do
    -- Logging/debugging
    writeFile "log" ""
    forkIO $ forever $ do
        s <- atomically $ readTQueue logger
        appendFile "log" (s ++ "\n")
    -- For each website
    forM_ urls initCrawler

