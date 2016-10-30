
-- Language extensions to enable

{-# LANGUAGE OverloadedStrings, TypeApplications,
             PartialTypeSignatures, TupleSections, BangPatterns,
             GADTs, RankNTypes, DeriveGeneric, DeriveAnyClass,
             TypeOperators
#-}

-- Module name
module Lib where


-- {{{ Imports
-- Helpful control structure function for failure/choice
import Control.Applicative ((<|>), empty)
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
                     , when, replicateM_, void
                     )
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.STM as STM
-- Convenient type-level operators
import Control.Type.Operator

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
import Data.Maybe (catMaybes, isJust, fromJust)
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
import qualified Data.Vector.Lens as Lens

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
type URL a = a

data Asset = Link !Text
           | Img !Text
           | Script !Text
           | Style !Text
           deriving (Eq, Ord, Show, Generic, NFData)

type (s |- b) = Tagged s b
infixr 9 |-
type (b -| s) = Tagged s b
infixr 9 -|

-- | Data structure(s) size
data Size

-- Asset tags
data Link
data Allowed
data Unvisited
data Stripped

data Recursive

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
               deriving (Show, Generic, NFData)

-- | URI path data structure
data URIPath =
    URIPath { uriPathPath :: !Text
            , uriPathGets :: Map Text Text
            }
            deriving (Eq, Show, Generic, NFData)

type AssetMap = Map Text $ Map Asset Int

unionSubs :: Map Asset Int -> Map Asset Int -> Map Asset Int
unionSubs = Map.unionWith (+)

insertAssets :: Text -> Map Asset Int -> AssetMap -> AssetMap
insertAssets = Map.insertWith unionSubs

-- | Crawler config data structure
data Config =
    Config { confDisPaths :: [Text]
           -- ^ Disallowed paths
           , confDomain :: !Text
           -- ^ Website domain
           , confHomePage :: URL String
           -- ^ Homepage URL String
           , confSession :: !Wreq.Session
           -- ^ Re-used Wreq session
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

-- | Download counter.
counterVar :: TVar Int
{-# NOINLINE counterVar #-}
counterVar = unsafePerformIO $ newTVarIO 0

succCounter = atomically $ do
    n <- readTVar counterVar
    writeTVar counterVar (n+1)
    return (n+1)


-- | Terminal printer thread's queue
printQueue :: TQueue String
{-# NOINLINE printQueue #-}
printQueue = unsafePerformIO newTQueueIO


-- | Terminal 'printer' front-end
printer :: String -> IO ()
printer = atomically . writeTQueue printQueue


-- | Futures/promises.
async :: IO a -> IO $ TMVar a
async io = do
    t <- newEmptyTMVarIO
    forkIO $ io >>= atomically . putTMVar t
    return t

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
getPage :: Wreq.Session -> URL String -> IO $ Maybe Taggy.Node
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
getPageAssets :: Taggy.Node -> Vector Asset
getPageAssets !node =
        -- Find all <a> and return href attributes
    let links :: Taggy.Node -> Vector Asset
        links n = flip Lens.toVectorOf n
                $ Taggy.allNamed (only "a")
                . Taggy.attr "href"
                . _Just
                . to Link
        -- Find all <img> and return src attributes
        imgs n = flip Lens.toVectorOf n
               $ Taggy.allNamed (only "img")
               . Taggy.attr "src"
               . _Just
               . to Img
        -- Find all <script> and return src attributes
        scripts n = flip Lens.toVectorOf n
                  $ Taggy.allNamed (only "script")
                  . Taggy.attr "src"
                  . _Just
                  . to Script
        -- Find all <link rel="stylesheet"> and return href attributes
        styles n = flip Lens.toVectorOf n
                 $ Taggy.allNamed (only "link")
                 . Taggy.attributed (ix "rel" . only "stylesheet")
                 . Taggy.attr "href"
                 . _Just
                 . to Style

    -- Monoid function composition
    in force $ (links <> imgs <> scripts <> styles) node


noExceptions _ _ _ = Nothing

    -- Don't throw exceptions on HTTP codes
opts = Wreq.defaults { Wreq.checkStatus = Just noExceptions }
    -- Make the crawler identifiable with a unique User-Agent
     & set (Wreq.header "User-Agent") ["milk-biscuit-teacake"]

-- | Download and return robots.txt disallows
getRobotRules :: Wreq.Session -> URL String -> IO [RobotRules]
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


-- {{{ Crawler

-- |
assetsToLinkSet :: Config -> AssetMap -> Set $ URL Text
assetsToLinkSet config !assetMap =
    let links = map fromAsset . filter isLink . join . fmap Map.keys
              $ Map.elems assetMap
        domain = confDomain config
        disPath = confDisPaths config
    in Set.fromList $ do
        link <- links
        let muri = maybeParseURI link
            isDomain = maybe True id $ uriVerifyDomain domain <$> muri
        guard isDomain
        -- TODO FIXME this is a DUMB hacc
        let !noFragmentLink = Text.takeWhile (/= '#') link
        let path = maybe noFragmentLink uriPath muri
            isAllowed = not $ any (`Text.isPrefixOf` path) disPath
        guard isAllowed
        return path

-- XXX has potential for optimization
sequenceDownlader :: Config -> AssetMap -> Set $ URL Text -> IO AssetMap
sequenceDownlader !config !assetMap !linkSet = do
    !newAssetMap <- batchDownloader config linkSet

        -- Accumulated asset map
    let !unionAssetMap = Map.unionWith unionSubs newAssetMap assetMap
        -- Accumulated link set
        !unionLinkSet = Set.fromList $ Map.keys unionAssetMap
        -- Link set from newly visited pages
        !newLinkSet = assetsToLinkSet config newAssetMap
        -- Newly introduced uncrawled links
        !diffLinkSet = Set.difference newLinkSet unionLinkSet

    print $ Set.size diffLinkSet

    -- Any more URLs to crawl?
    if Set.null diffLinkSet
    -- Finish up
    then return unionAssetMap
    -- Keep downloading batches
    else sequenceDownlader config unionAssetMap diffLinkSet

batchDownloader :: Config -> Set $ URL Text -> IO AssetMap
batchDownloader config linkSet = do
    futures <- forM (Set.toList linkSet) $ async
                                         . downloader config
                                         . Text.unpack

    -- Returns when all futures have been read
    Map.unionsWith unionSubs <$> mapM (atomically . readTMVar) futures

downloader :: Config -> URL String -> IO AssetMap
downloader config path = do
    let !url = confHomePage config </> dropDrive path

    !mnode <- getPage (confSession config) url

    let !assetMap = Map.singleton (Text.pack path) $ Map.fromList
                  $ map (,1) $ Vec.toList
                  $ maybe Vec.empty getPageAssets mnode

    --succCounter >>= printer . printf "Counter %d\n"

    return assetMap


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
writeSiteMap filename assetMap = do
    let formatSub prevText asset freq = mconcat
            [ prevText, "\tFrequency: ", Text.pack (show freq)
            , ";\tAsset: ", Text.pack $ show asset, "\n"
            ]
        format prevText url subMap = mconcat
            [prevText, url, "\n", showSubMap subMap, "\n"]

        showSubMap = Map.foldlWithKey formatSub ""
        showMap = Map.foldlWithKey format "" assetMap

    Text.writeFile (Text.unpack filename) showMap

printStats :: _ -> String -> _
printStats url total assetMap startTime httpTime = do
    -- Print basic stats
    printf "\nFinished with %s.\n" url
    printf "\tTotal threads: %s\n" total
    let linkSet = Set.fromList $ Map.keys assetMap
    printf "\tURLs crawled: %d\n" (Set.size linkSet)
    let links = filter isLink . List.nub . join . Map.elems
              $ Map.map Map.keys assetMap
    printf "\tUnique asset links: %d\n" (length links)
    let imgs = filter isImg . List.nub . join . Map.elems
             $ Map.map Map.keys assetMap
    printf "\tUnique asset imgs: %d\n" (length imgs)
    let scripts = filter isScript . List.nub . join . Map.elems
                $ Map.map Map.keys assetMap
    printf "\tUnique asset scripts: %d\n" (length scripts)
    let styles = filter isStyle . List.nub . join . Map.elems
               $ Map.map Map.keys assetMap
    printf "\tUnique asset styles: %d\n" (length styles)
    endTime <- getCurrentTime
    printf "\tTime elapsed (HTTP): %s\n"
        (show $ diffUTCTime httpTime startTime)
    printf "\tTime elapsed (total): %s\n"
        (show $ diffUTCTime endTime startTime)

initCrawler _ url = Wreq.withAPISession $ \session -> do
    printf "Crawling website %s\n" url
    startTime <- getCurrentTime

    robotRules <- getRobotRules session url
    let disallowedPaths = do
            -- Filter non-RobotDisallowed
            RobotDisallowed x <- robotRules
            return $! x

    putStr "Disallowed paths: " >> print disallowedPaths

    let domain = maybe "" uriDomain . maybeParseURI $ Text.pack url

        config = Config disallowedPaths domain url session

    printf "Downloading initial asset map.\n"
    initAssetMap <- downloader config "/"

    let linkSet = assetsToLinkSet config initAssetMap

    printf "Starting sequence downloader.\n"
    assetMap <- sequenceDownlader config initAssetMap linkSet

    printStats url "Infinity" assetMap startTime startTime

    -- Write site map
    let siteMapPath = "assets_" <> domain <> ".txt"
    printf "Writing site map %s" siteMapPath
    writeSiteMap siteMapPath assetMap


-- | Consistently sequential terminal output thread
printerThread = forkIO $ forever $ do
    atomically (peekTQueue printQueue) >>= putStr
    atomically $ readTQueue printQueue


initProgram :: Args -> IO ()
initProgram args = do
    let urls = argsURLs args
        threadCount = argsThreads args

    printerThread

    -- For each website
    forM_ urls $ initCrawler threadCount

