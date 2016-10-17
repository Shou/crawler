
-- Language extensions to enable

{-# LANGUAGE OverloadedStrings, TypeApplications,
             PartialTypeSignatures, TupleSections, BangPatterns,
             GADTs, RankNTypes, DeriveGeneric, DeriveAnyClass
#-}

-- Module name
module Lib where


-- {{{ Imports
-- Helpful control structure function for failure/choice
import Control.Applicative ((<|>))
-- Concurrency
import Control.Concurrent (forkIO, myThreadId, ThreadId)
-- Mutable shared state
import Control.Concurrent.MVar
-- Non-blocking mutable shared state
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
-- Deep seq
import Control.DeepSeq
-- Exception handling
import Control.Exception (try, SomeException)
-- Helpful control structure functions for dealing with nested data
import Control.Lens
-- Helpful control structure functions
import Control.Monad ( forM, forM_, forever, guard, unless, join
                     , when
                     )
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.STM as STM (retry)

-- Efficient textual parser
import qualified Data.Attoparsec.Text as Atto
-- Efficient, low-level String
import qualified Data.ByteString as BS
-- Lazily evaluated ByteString
import qualified Data.ByteString.Lazy as BL
-- Boolean utilities
import Data.Bool (bool)
-- Case insensitive text
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
-- Safe type-coercion
import Data.Coerce (coerce)
-- Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.TQueue
-- Function utilities
import Data.Function (on)
-- List utilities
import qualified Data.List as List
-- Key/value map data structure
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- Safe failures
import Data.Maybe (catMaybes)
-- Monoids: appendable data structures
import Data.Monoid ((<>))
-- Sets
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
-- Efficient UTF8 String
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text (decodeUtf8With)
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Data.Text.Lazy.Encoding as LazyText (decodeUtf8With)
-- Time utilities
import Data.Time (getCurrentTime, diffUTCTime)
-- Vector: efficient arrays
import Data.Vector (Vector)
import qualified Data.Vector as Vec

-- Generic data
import GHC.Generics (Generic)

-- Wreq is for HTTP requests
import qualified Network.Wreq as Wreq (responseBody, defaults, header)
import qualified Network.Wreq.Session as Wreq
import qualified Network.Wreq.Types as Wreq

-- Safe(r) paths; we use this for URL construction
import System.FilePath.Posix ((</>), dropDrive)
-- "Unsafe" IO functions; only used safely here!
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)

-- XML parser
import qualified Text.Taggy as Taggy hiding (htmlWith)
import qualified Text.Taggy.Lens as Taggy
-- }}}


-- {{{ Data and types

-- URLs are just Strings
type URL = String

data Asset = Link Text
           | Img Text
           | Script Text
           | Style Text
           deriving (Ord, Show, Generic, NFData)

newtype AssetOf tag = AssetOf Asset

data Link
data Allowed
data Unvisited
data Stripped

-- | Ignore the constructor
instance Eq Asset where
    t1 == t2 = fromAsset t1 == fromAsset t2

mapAsset f (Link t) = Link $ f t
mapAsset f (Img t) = Img $ f t
mapAsset f (Script t) = Script $ f t
mapAsset f (Style t) = Style $ f t

fromAsset :: Asset -> Text
fromAsset (Link t) = t
fromAsset (Img t) = t
fromAsset (Script t) = t
fromAsset (Style t) = t

isLink :: Asset -> Bool
isLink (Link _) = True
isLink _ = False

isImg :: Asset -> Bool
isImg (Img _) = True
isImg _ = False

isScript :: Asset -> Bool
isScript (Script _) = True
isScript _ = False

isStyle :: Asset -> Bool
isStyle (Style _) = True
isStyle _ = False

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

-- | URI path data structure
data URIPath =
    URIPath { uriPathPath :: Text
            , uriPathGets :: Map Text Text
            }
            deriving (Eq, Show)

type AssetMap = Map Text (Map Asset Int)

mergeSubAssets :: Map Asset Int -> Map Asset Int
               -> Map Asset Int
mergeSubAssets = Map.unionWith (+)

insertAssets :: Text -> Map Asset Int -> AssetMap -> AssetMap
insertAssets = Map.insertWith mergeSubAssets

-- | Crawler config data structure
data Config =
    Config { confDisPaths :: [Text]
           -- ^ Disallowed paths
           , confDomain :: Text
           -- ^ Website domain
           , confAssetCache :: TVar AssetMap
           -- ^ Static assets, and their associated visitor URLs and frequency
           }

-- Used in Main
-- | Command line arguments data structure
data Args = Args { argsThreads :: Int
                 -- ^ Amount of threads
                 , argsURLs :: [String]
                 -- ^ Websites to crawl
                 }

-- }}}


-- {{{ Shared state

-- | Visited link cache
linkCache :: TVar (Set Text)
linkCache = unsafePerformIO $ newTVarIO Set.empty

-- | Unvisited link queue
linkQueue :: TQueue Text
linkQueue = unsafePerformIO newTQueueIO

-- | Quantity of finished threads
threadDoneCache :: TVar Int
threadDoneCache = unsafePerformIO $ newTVarIO 0

-- }}}


-- {{{ Parsers

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

uriPathParser :: Atto.Parser URIPath
uriPathParser = Atto.choice [ pathAndGets
                            , URIPath <$> Atto.takeText <*> pure Map.empty
                            ]
  where
    pathAndGets = do
        path <- (Atto.takeWhile1 (/= '?') <|> pure "") <* Atto.char '?'
        gets <- Atto.many' $ (,) <$> (Atto.takeWhile1 (/= '=') <* Atto.char '=')
                                 <*> ((Atto.takeWhile1 (/= '&') <* Atto.char '&') <|> Atto.takeText)
        return $ URIPath path (Map.fromList gets)

-- | Parse a path string into a Maybe URIPath
maybeParseURIPath :: Text -> Maybe URIPath
maybeParseURIPath path = Atto.maybeResult
                       $ Atto.parse uriPathParser path `Atto.feed` ""

-- | Parse a URI string into a Maybe URI
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

-- }}}


-- {{{ Web utils

-- | Download a webpage and return the parsed contents
getPage :: Wreq.Session -> URL -> IO (Maybe Taggy.Node)
getPage session url = do
    request <- try @SomeException $ Wreq.getWith opts session url

    let body :: LazyText.Text
        body = request ^. _Right
                        -- Get the response body
                        . Wreq.responseBody
                        -- Lenient UTF-8 decoding to Text
                        . to (LazyText.decodeUtf8With Text.lenientDecode)

    return $ body ^? Taggy.htmlWith False

-- | Parse HTML into contained asset links
getPageAssets :: Taggy.Node -> [Asset]
getPageAssets !node =
        -- Find all <a> and return href attributes
    let links = map Link . catMaybes $ do
            node ^.. Taggy.allNamed (only "a") . Taggy.attr "href"
        -- Find all <img> and return src attributes
        imgs = map Img . catMaybes $ do
            node ^.. Taggy.allNamed (only "img") . Taggy.attr "src"
        -- Find all <script> and return src attributes
        scripts = map Script . catMaybes $ do
            node ^.. Taggy.allNamed (only "script") . Taggy.attr "src"
        -- Find all <link rel="stylesheet"> and return href attributes
        styles = map Style . catMaybes $ do
            node ^.. Taggy.allNamed (only "link")
                   . Taggy.attributed (ix "rel" . only "stylesheet")
                   . Taggy.attr "href"

    in links <> imgs <> scripts <> styles


noExceptions _ _ _ = Nothing

    -- Don't throw exceptions on HTTP codes
opts = Wreq.defaults { Wreq.checkStatus = Just noExceptions }
    -- Make the crawler identifiable with a unique User-Agent
     & set (Wreq.header "User-Agent") ["milk-biscuit-teacake"]

-- | Download and return robots.txt disallows
getRobotRules :: Wreq.Session -> URL -> IO [RobotRules]
getRobotRules session host = do
    let url = host </> "robots.txt"
    request <- try @SomeException $ Wreq.getWith opts session url

    let body :: BL.ByteString
        body = view (_Right . Wreq.responseBody) request

        textBody :: Text
        textBody = Text.decodeUtf8With Text.lenientDecode
                                       (BL.toStrict body)

              -- Filter ignored lines
    let rules = filter (/= RobotIgnored)
              -- Default to empty list on failure
              . maybe [] id . Atto.maybeResult
              -- Parse the robots.txt file
              $ Atto.parse robotParser textBody `Atto.feed` ""

    return rules


-- FIXME protocol links e.g.
--          mailto:hello@test.com
--          javascript:alert(1);
parseURIC :: Text -> Conduit (AssetOf Link) IO (AssetOf Stripped)
parseURIC !domain = awaitForever $ \asset -> do
    let link = fromAsset $ coerce asset

    -- Parse link to Maybe URI
    let muri = maybeParseURI link
        -- Verify domain; assume no domain on parse failure
        isDomain = maybe True id $ uriVerifyDomain domain <$> muri

    when isDomain $ do
            -- Remove fragment from link
        let noFragmentLink = Text.takeWhile (/= '#') link
            -- Use link as path instead of uriPath on parse failure
            path = maybe noFragmentLink uriPath muri

        yield $ AssetOf $ Link path

removeDisallowedC :: [Text] -> Conduit (AssetOf Stripped) IO
                                       (AssetOf Allowed)
removeDisallowedC !disPaths = awaitForever $ \asset -> do
    let path = fromAsset $ coerce asset

    -- Filter disallowed paths
    let isAllowed = not $ any (`Text.isPrefixOf` path) disPaths
    when isAllowed $ yield $ AssetOf $ Link path

removeVisitedC :: Conduit (AssetOf Allowed) IO (AssetOf Unvisited)
removeVisitedC = awaitForever $ \asset -> do
    let path = fromAsset $ coerce asset

    -- Get visited links from the cache
    visitedLinks <- liftIO $ readTVarIO linkCache
    -- Filter visited paths
    when (not $ Set.member path visitedLinks) $ do
        yield $ AssetOf $ Link path

linkQueueC :: Sink (AssetOf Allowed) IO ()
linkQueueC = do
    removeVisitedC =$= CC.map (fromAsset . coerce)
                   =$= sinkTQueue linkQueue

pageAssetsP :: Wreq.Session -> URL
            -> Producer IO Asset
pageAssetsP session url = do
    -- Download page
    mnode <- liftIO $ getPage session url
    -- Get page assets; return empty assets on getPage parse failure
    CC.yieldMany $ maybe [] getPageAssets mnode

pageLinksC :: Conduit Asset IO (AssetOf Link)
pageLinksC = CC.mapWhile f
  where
    f a = bool Nothing (Just $ AssetOf a) (isLink a)

-- FIXME ensure same domain on links
-- FIXME strip GET parameters from URLs
crawl :: Wreq.Session -> URL -> Config -> ConduitM _ _ _ _
crawl session url config = do
    let assetCache = confAssetCache config

    let zipsC = [mergeAssetsC url assetCache, addLinksC config]
    pageAssetsP session url =$= sequenceConduits zipsC

    return ()

addLinksC :: Config -> Conduit Asset IO _
addLinksC config = do
    let disallowedPaths = confDisPaths config
        domain = confDomain config

    pageLinksC =$= parseURIC domain
               =$= removeDisallowedC disallowedPaths
               =$= sequenceConduits [ linkQueueC, linkCacheC ]

    return ()

-- ()
finishWork :: Bool -> IO ()
finishWork _ = do
    -- This work finished
    atomically $ modifyTVar' threadDoneCache (+1)

-- Merge assets and ????
mergeAssetsC :: URL -> _ -> Consumer Asset IO ()
mergeAssetsC url assetCache = awaitForever $ \asset -> do
    liftIO . atomically $ do
        let merger :: Asset -> AssetMap -> AssetMap
            merger = insertAssets (Text.pack url)
                   . flip Map.singleton 1

        -- Add assets
        modifyTVar' assetCache $ merger asset

-- | Remove duplicate asset links
removeDuplicatesC :: Set Asset
                  -> Conduit (AssetOf _) IO (AssetOf _)
removeDuplicatesC s = do
    ma <- coerce <$> await

    flip (maybe $ return ()) ma $ \a ->
        if Set.member a s
        then removeDuplicatesC s
        else do
            yield $ AssetOf a
            removeDuplicatesC $ Set.insert a s

-- | Add to the link cache
linkCacheC :: Conduit (AssetOf Allowed) IO _
linkCacheC = awaitForever $ \asset -> do
    let path = fromAsset $ coerce asset
    liftIO . atomically $ do
        -- Add paths that we've seen and will queue
        modifyTVar' linkCache $ Set.insert path


-- | Re-use HTTP session to crawl
sessionHandler :: URL -> Config -> IO ()
sessionHandler homepage config = do
    -- No-cookie HTTP sessions
    Wreq.withAPISession $ \session -> forever $ do
        -- Read next link
        path <- atomically $ Text.unpack <$> readTQueue linkQueue
        -- Thread unfinished; remove done status
        atomically $ modifyTVar' threadDoneCache (max 0 . subtract 1)
        -- Safely combine homepage and path to a URL
        let url = homepage </> dropDrive path
        -- Crawl page for more links
        runConduit $ addCleanup finishWork $ crawl session url config
        -- Progress
        putChar 'o'

-- }}}


-- | Write the sitemap to a formatted text file
writeSiteMap :: Text -> AssetMap -> IO ()
writeSiteMap domain assetMap = do
    let formatSub prevText asset freq = mconcat
            [ prevText, "\tFrequency: ", Text.pack (show freq)
            , ";\tAsset: ", Text.pack $ show asset, "\n"
            ]
        format prevText url subMap = mconcat
            [prevText, url, "\n", showSubMap subMap, "\n"]

        showSubMap = Map.foldlWithKey formatSub ""
        showMap = Map.foldlWithKey format "" assetMap

    Text.writeFile (Text.unpack domain) showMap


-- | Initialise the crawler with concurrent sessionHandlers
initCrawler :: Int -> URL -> IO ()
initCrawler threadCount url = do
    putStrLn $ "Crawling \"" ++ url ++ "\" with "
                             ++ show threadCount ++ " threads."

    -- Start time
    !startTime <- getCurrentTime

    let domain = maybe "" uriDomain . maybeParseURI $ Text.pack url

    -- Get initial homepage links, and robots.txt disallowed paths
    configs <- Wreq.withAPISession $ \session -> do
        robotRules <- getRobotRules session url

        let !disallowedPaths = do
                -- Filter non-RobotDisallowed
                RobotDisallowed (!x) <- robotRules
                return x

        configs <- forM [1 .. threadCount] $ const $ do
            assetCache <- newTVarIO Map.empty
            return $ Config disallowedPaths domain assetCache

        case configs of
            (config:_) -> runConduit $ crawl session url config
            _ -> putStrLn "No threads."

        return configs

    -- Run sessionHandlers in separate threads
    forM_ configs $ forkIO . sessionHandler url

    -- Done threads list
    dones <- atomically $ do
        dones <- readTVar threadDoneCache
        -- All threads are currently done, we may be finished
        if dones == threadCount then do
            isEmptyLinkQueue <- isEmptyTQueue linkQueue
            -- When the link queue is empty, we're done; otherwise retry
            unless isEmptyLinkQueue STM.retry

        -- Only some threads are done; retry
        else STM.retry

        return dones

    !httpTime <- getCurrentTime

    -- Collect assets from threads' shared state
    allAssets <- fmap (Map.unionsWith mergeSubAssets)
              . forM configs $ readTVarIO . confAssetCache

    -- Write site map
    writeSiteMap domain allAssets

    -- Print basic stats
    putChar '\n'
    putStrLn $ "Finished with \"" ++ url ++ "\"."
    putChar '\t'
    putStrLn $ "Done threads: " ++ show dones
    putChar '\t'
    linkSet <- readTVarIO linkCache
    putStrLn $ "URLs crawled: " ++ show (Set.size linkSet)
    putChar '\t'
    let links = filter isLink . List.nub . join . Map.elems
              $ Map.map Map.keys allAssets
    putStrLn $ "Unique asset links: " ++ show (length links)
    putChar '\t'
    let imgs = filter isImg . List.nub . join . Map.elems
             $ Map.map Map.keys allAssets
    putStrLn $ "Unique asset imgs: " ++ show (length imgs)
    putChar '\t'
    let scripts = filter isScript . List.nub . join . Map.elems
                $ Map.map Map.keys allAssets
    putStrLn $ "Unique asset scripts: " ++ show (length scripts)
    putChar '\t'
    let styles = filter isStyle . List.nub . join . Map.elems
               $ Map.map Map.keys allAssets
    putStrLn $ "Unique asset styles: " ++ show (length styles)
    putChar '\t'
    endTime <- getCurrentTime
    putStrLn $ "Time elapsed (HTTP): " ++ show (diffUTCTime httpTime startTime)
    putChar '\t'
    endTime <- getCurrentTime
    putStrLn $ "Time elapsed (total): " ++ show (diffUTCTime endTime startTime)
    putStrLn $ "Sitemap file: " ++ Text.unpack domain

    -- Exit program
    exitSuccess
    return ()

initProgram :: Args -> IO ()
initProgram args = do
    let urls = argsURLs args
        threadCount = argsThreads args
    -- For each website
    forM_ urls $ initCrawler threadCount

