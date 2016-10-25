
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
import Control.Concurrent.STM.TMVar
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
                     , when, replicateM_
                     )
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.STM as STM

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
import Data.Maybe (catMaybes, isJust)
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
-- Program exit
import System.Exit (exitSuccess)
-- "Unsafe" IO functions; only used safely here!
import System.IO.Unsafe (unsafePerformIO)
-- Random values
import System.Random (randomRIO)

-- Blaze HTML parser
import qualified Text.Blaze.Html as Blaze
-- HTML conduit
import qualified Text.HTML.DOM as HTML
-- String formatting/printing
import Text.Printf (printf)
-- Taggy HTML parser
import qualified Text.Taggy as Taggy hiding (htmlWith)
import qualified Text.Taggy.Lens as Taggy
-- Lenses for 'XML'
import qualified Text.XML.Lens as XML
-- }}}


-- {{{ Data and types

-- URLs are just Strings
type URL = String

data Asset = Link !Text
           | Img !Text
           | Script !Text
           | Style !Text
           deriving (Eq, Ord, Show, Generic, NFData)

-- | Tagged asset
newtype AssetOf tag = AssetOf Asset

-- | Data structure(s) size
newtype Count a = Count Int

data Link
data Allowed
data Unvisited
data Stripped

-- {{{ Asset utils

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

-- }}}

-- | Data structure for Robots.txt rules
data RobotRules = RobotUserAgent !Text
                | RobotDisallowed !Text
                | RobotIgnored
                deriving (Show, Eq)

-- | URI data structure
data URI = URI { uriProtocol :: !Text
               , uriUserinfo :: !Text
               , uriDomain :: !Text
               , uriPort :: !Text
               , uriPath :: !Text
               }
               deriving (Show)

-- | URI path data structure
data URIPath =
    URIPath { uriPathPath :: !Text
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
           , confDomain :: !Text
           -- ^ Website domain
           , confAssetCache :: TVar AssetMap
           -- ^ Static assets, and their associated visitor URLs and frequency
           , confHomePage :: !URL
           -- ^ Homepage URL String
           , confSession :: !Wreq.Session
           -- ^ Re-used Wreq session
           , confThreads :: Maybe (Count ThreadId)
           -- ^ Integer count of threads
           }

-- Used in Main
-- | Command line arguments data structure
data Args = Args { argsThreads :: Maybe Int
                 -- ^ Amount of threads
                 , argsURLs :: [String]
                 -- ^ Websites to crawl
                 }

-- }}}


-- {{{ Concurrency and shared state

-- | Mirrored (visited) link cache
linkMirror :: TMirror (Set Text)
linkMirror = unsafePerformIO $ newTMirrorIO 10 Set.empty

-- | Unvisited link queue
linkQueue :: TQueue Text
linkQueue = unsafePerformIO newTQueueIO

-- | Full list of asset cache TVars
assetCachesVar :: TVar [TVar AssetMap]
assetCachesVar = unsafePerformIO $ newTVarIO []

-- | Terminal printer thread's queue
printQueue :: TQueue String
printQueue = unsafePerformIO newTQueueIO

-- | Active threads counter
activeThreadsVar :: TVar Int
activeThreadsVar = unsafePerformIO $ newTVarIO 0

-- | Total threads counter
totalThreadsVar :: TVar Int
totalThreadsVar = unsafePerformIO $ newTVarIO 0

-- | Finished threads counter
doneThreadsVar :: TVar Int
doneThreadsVar = unsafePerformIO $ newTVarIO 0

keyQuitVar :: TMVar ()
keyQuitVar = unsafePerformIO newEmptyTMVarIO


-- | Efficient concurrent atomic reads
newtype TMirror a = TMirror { unMirror :: Vector (TVar a) }

newTMirrorIO :: NFData a => Int -> a -> IO (TMirror a)
newTMirrorIO len a = do
    fmap TMirror . Vec.replicateM len $ newTVarIO (force a)

readTMirrorIO :: NFData a => TMirror a -> IO a
readTMirrorIO tm = do
    n <- randomRIO (0, Vec.length (unMirror tm) - 1)
    fmap force . readTVarIO $ unMirror tm Vec.! n

modifyTMirrorIO :: TMirror a -> (a -> a) -> IO ()
modifyTMirrorIO tm f = mapM_ (atomically . flip modifyTVar' f) vtm
  where
    vtm = unMirror tm


-- | Terminal 'printer' front-end
printer :: String -> IO ()
printer = atomically . writeTQueue printQueue


async io = do
    t <- newEmptyTMVarIO
    forkIO $ io >>= atomically . putTMVar t
    return t

readAsync = atomically . readTMVar

asyncStore t io = atomically . modifyTVar' t . (:) =<< async io

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

-- FIXME Stack space overflow
-- Text.Taggy.DOM.untilClosed.ts'',
-- called from Text.Taggy.DOM.untilClosed,
-- called from Text.Taggy.DOM.domify.(...),
-- called from Text.Taggy.DOM.domify,
-- called from Text.Taggy.Lens.htmlWith.parse,
-- called from Text.Taggy.Lens.htmlWith,
-- called from Lib.getPage
-- | Download a webpage and return the parsed contents
getPage :: Wreq.Session -> URL -> IO (Maybe Taggy.Node)
getPage session url = do
    !startTime <- getCurrentTime
    request <- try @SomeException $ Wreq.getWith opts session url
    !endTime <- getCurrentTime

    let byteBody = request ^. _Right . Wreq.responseBody
        body :: LazyText.Text
        body = request ^. _Right
                        -- Get the response body
                        . Wreq.responseBody
                        -- Lenient UTF-8 decoding to Text
                        . to (LazyText.decodeUtf8With Text.lenientDecode)

    printer $
        printf "(%dkB, %s): %s\n"
            (BL.length byteBody `div` 1000)
            (show $ diffUTCTime endTime startTime)
            url

    return $ body ^? Taggy.htmlWith False

-- FIXME Stack space overflow
-- Text.Taggy.Lens.element,
-- called from Text.Taggy.Lens.elements,
-- called from Text.Taggy.Lens.plate,
-- called from Text.Taggy.Lens.allNamed,
-- called from Lib.getPageAssets.imgs
-- | Parse HTML into contained asset links
getPageAssets :: Taggy.Node -> [Asset]
getPageAssets !node =
        -- Find all <a> and return href attributes
    let links n = map Link . catMaybes $ do
            n ^.. Taggy.allNamed (only "a") . Taggy.attr "href"
        -- Find all <img> and return src attributes
        imgs n = map Img . catMaybes $ do
            n ^.. Taggy.allNamed (only "img") . Taggy.attr "src"
        -- Find all <script> and return src attributes
        scripts n = map Script . catMaybes $ do
            n ^.. Taggy.allNamed (only "script") . Taggy.attr "src"
        -- Find all <link rel="stylesheet"> and return href attributes
        styles n = map Style . catMaybes $ do
            n ^.. Taggy.allNamed (only "link")
                . Taggy.attributed (ix "rel" . only "stylesheet")
                . Taggy.attr "href"

    -- Monoid function composition
    in force $ (links <> imgs <> scripts <> styles) node


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

-- }}}


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

-- | Add to the link cache
addUnvisitedC :: Set Text
               -> Conduit (AssetOf Allowed) IO (AssetOf Unvisited)
addUnvisitedC unvisited = do
    -- Get visited links from the mirrored cache
    visited <- liftIO $ readTMirrorIO linkMirror

    ma <- await
    case ma of
        Just asset -> do
            let !path = fromAsset $ coerce asset

            -- Filter visited paths
            if Set.notMember path (visited <> unvisited)

            then do
                yield (AssetOf $ Link path)
                addUnvisitedC $ Set.insert path unvisited

            else addUnvisitedC unvisited

        Nothing -> liftIO $ do
            -- Add unvisited links to mirrored link cache
            modifyTMirrorIO linkMirror (Set.union unvisited)


linkQueueC :: Sink (AssetOf Unvisited) IO ()
linkQueueC = do
    CC.map (fromAsset . coerce) =$= sinkTQueue linkQueue

pageAssetsP :: Config -> URL
            -> Producer IO Asset
pageAssetsP config url = do
    let session = confSession config
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
crawl :: Config -> URL -> Sink i IO ()
crawl config url = do
    let assetCache = confAssetCache config

    let zipsC = [mergeAssetsC url assetCache, addLinksC config]
    pageAssetsP config url =$= sequenceConduits zipsC

    return ()

-- |
balanceC :: Sink (AssetOf Unvisited) IO ()
balanceC = liftIO $ do
    -- This thread now done and soon dead
    atomically $ do
        modifyTVar' activeThreadsVar (max 0 . subtract 1)
        modifyTVar' doneThreadsVar (+1)

addLinksC :: Config -> Sink Asset IO ()
addLinksC config = do
    let disallowedPaths = confDisPaths config
        domain = confDomain config

    let linkSequences = [linkQueueC, balanceC]
    pageLinksC =$= parseURIC domain
               =$= removeDisallowedC disallowedPaths
               =$= addUnvisitedC mempty
               =$= sequenceConduits linkSequences

    return ()

-- Merge assets and ????
mergeAssetsC :: URL -> TVar AssetMap -> Consumer Asset IO ()
mergeAssetsC url assetCache = awaitForever $ \asset -> do
    liftIO . atomically $ do
        let merger :: Asset -> AssetMap -> AssetMap
            merger = insertAssets (Text.pack url)
                   . flip Map.singleton 1
                   . force

        -- Add assets
        modifyTVar' assetCache $ merger asset


-- | Re-use HTTP session to crawl
threadHandler :: Config -> IO ()
threadHandler config = forever $ do
    -- Read next link
    path <- atomically $ Text.unpack <$> readTQueue linkQueue

    -- Thread (still) active
    (total, active) <- atomically $ do
        total <- readTVar totalThreadsVar
        active <- readTVar activeThreadsVar

        let mayMaxThreads = coerce <$> confThreads config
        -- Is maxThreads set?
        case mayMaxThreads of
            -- Too many threads? Retry
            Just maxThreads -> unless (active < maxThreads) STM.retry
            Nothing -> return ()

        modifyTVar' totalThreadsVar (+1)
        modifyTVar' activeThreadsVar (+1)

        return (total + 1, active + 1)

    printer $ printf "Threads: %d total, %d active\n" total active

    -- Safely combine homepage and path to a URL
    let url = confHomePage config </> dropDrive path
    -- Crawl page for more links
    forkIO $ runConduit $ crawl config url

-- }}}


-- FIXME Stack space overflow
-- Data.Text.IO.writeFile,
-- called from Data.Text.concat.ts',
-- called from Data.Text.concat,
-- called from Lib.writeSiteMap.format,
-- called from Lib.writeSiteMap.showMap,
-- called from Lib.writeSiteMap
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


-- 'Blocks'/retries until we're done
areWeDoneYet = atomically $ do
    active <- readTVar activeThreadsVar
    total <- readTVar totalThreadsVar
    dones <- readTVar doneThreadsVar
    -- All threads are currently done, we may be finished
    if active == 0 && dones > 1 && dones == total then do
        isEmptyLinkQueue <- isEmptyTQueue linkQueue
        isKeyQuit <- isJust <$> tryReadTMVar keyQuitVar
        -- When the link queue is empty, we're done; otherwise retry
        unless (isEmptyLinkQueue || isKeyQuit) STM.retry

    -- Only some threads are done; retry
    else do
        -- Manual keyboard quit?
        isKeyQuit <- isJust <$> tryReadTMVar keyQuitVar
        -- Keep retrying unless we're manually quitting
        unless isKeyQuit STM.retry

    return total


-- TODO
-- | Initialise the crawler with concurrent sessionHandlers
initCrawler :: Maybe Int -> URL -> IO ()
initCrawler mayThreadLimit url = Wreq.withAPISession $ \session -> do
    printf "Crawling %s.\n" url

    -- Start time
    !startTime <- getCurrentTime

    -- Get initial homepage links, and robots.txt disallowed paths
    robotRules <- getRobotRules session url

    let disallowedPaths = do
            -- Filter non-RobotDisallowed
            RobotDisallowed x <- robotRules
            return $! x

    assetCache <- newTVarIO Map.empty

    let domain = maybe "" uriDomain . maybeParseURI $ Text.pack url
        threadCount = fmap coerce mayThreadLimit
        config = Config disallowedPaths domain assetCache url session
                        threadCount

    -- Queue home page for downloading
    atomically $ writeTQueue linkQueue ""
    -- Run initial thread handler
    forkIO $ threadHandler config

    -- Terminal output thread
    forkIO $ forever $ do
        atomically (peekTQueue printQueue) >>= putStr
        atomically $ readTQueue printQueue

    -- Keyboard input
    forkIO $ forever $ do
        input <- getLine
        when (input == ":q") $ atomically $ putTMVar keyQuitVar ()

    total <- areWeDoneYet

    !httpTime <- getCurrentTime

    -- Collect assets from threads' shared state
    allAssets <- atomically $ do
        cacheVars <- readTVar assetCachesVar
        assets <- forM cacheVars readTVar
        return $ Map.unionsWith mergeSubAssets assets

    -- Write site map
    --writeSiteMap domain allAssets

    -- Synchronize with 'printer'
    atomically $ do
        b <- isEmptyTQueue printQueue
        unless b STM.retry

    linkSet <- readTMirrorIO linkMirror

    -- Print basic stats
    printf "\nFinished with %s.\n" url
    printf "\tDone threads: %d\n" total
    printf "\tURLs crawled: %d\n" (Set.size linkSet)
    let links = filter isLink . List.nub . join . Map.elems
              $ Map.map Map.keys allAssets
    printf "\tUnique asset links: %d\n" (length links)
    let imgs = filter isImg . List.nub . join . Map.elems
             $ Map.map Map.keys allAssets
    printf "\tUnique asset imgs: %d\n" (length imgs)
    let scripts = filter isScript . List.nub . join . Map.elems
                $ Map.map Map.keys allAssets
    printf "\tUnique asset scripts: %d\n" (length scripts)
    let styles = filter isStyle . List.nub . join . Map.elems
               $ Map.map Map.keys allAssets
    printf "\tUnique asset styles: %d\n" (length styles)
    endTime <- getCurrentTime
    printf "\tTime elapsed (HTTP): %s\n"
        (show $ diffUTCTime httpTime startTime)
    endTime <- getCurrentTime
    printf "\tTime elapsed (total): %s\n"
        (show $ diffUTCTime endTime startTime)
    printf "Sitemap file: %s\n" domain

    -- Exit program
    exitSuccess

initProgram :: Args -> IO ()
initProgram args = do
    let urls = argsURLs args
        threadCount = argsThreads args
    -- For each website
    forM_ urls $ initCrawler threadCount

