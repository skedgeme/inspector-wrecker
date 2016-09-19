{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, CPP #-}
module Wrecker.Inspector.HAR where
import Control.Exception
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Wrecker(Recorder, record)
import Data.Time.Clock 
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Text.Encoding
import Data.Maybe
import Data.Time.Calendar
import Data.Aeson
import Data.Either
import Control.Monad
import Network.HTTP.Client.TLS
import Data.Time.Format
import Network.Connection
import Data.Default

parseExpiry :: Text -> Either SomeException UTCTime
parseExpiry 
  = maybe (Left $ toException $ userError "can't format expires time") Right 
  . parseTimeM True defaultTimeLocale "%FT%T%QZ"
  . T.unpack

data HAR = HAR 
  { hLog :: Log
  } deriving (Show, Eq)
  
instance FromJSON HAR where
  parseJSON = withObject "HAR" $ \o -> do
    HAR <$> o .: "log"
  
data Log = Log
  { lEntries :: [Entry]
  } deriving (Show, Eq)
  
instance FromJSON Log where
  parseJSON = withObject "Log" $ \o -> do
    Log <$> o .: "entries" 
  
data Entry = Entry 
  { eRequest :: Request
  } deriving (Show, Eq)
  
instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \o -> do
    Entry <$> o .: "request"
  
data Request = Request
  { rMethod      :: Text
  , rUrl         :: Text
  , rHttpVersion :: Text
  , rHeaders     :: [Header]
  , rQueryString :: [QueryItem]
  , rCookies     :: [Cookie]
  , rPostData    :: Maybe PostData
  } deriving (Show, Eq)
  
instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    Request <$> o .: "method"
            <*> o .: "url"
            <*> o .: "httpVersion"
            <*> o .: "headers"
            <*> o .: "queryString"
            <*> o .: "cookies"
            <*> o .:? "postData" .!= Nothing
  
data Cookie = Cookie 
  { cName     :: Text
  , cValue    :: Text
  , cDomain   :: Maybe Text
  , cPath     :: Maybe Text
  , cExpires  :: Maybe Text
  , cHttpOnly :: Maybe Bool
  , cSecure   :: Maybe Bool
  } deriving (Show, Eq)

instance FromJSON Cookie where
  parseJSON = withObject "Cookie" $ \o -> do    
    Cookie <$> o .: "name"
           <*> o .: "value" 
           <*> o .:? "domain"   .!= Nothing 
           <*> o .:? "path"     .!= Nothing
           <*> o .:? "expires"  .!= Nothing
           <*> o .:? "httpOnly" .!= Nothing
           <*> o .:? "secure"   .!= Nothing

data Header = Header 
  { hName  :: Text
  , hValue :: Text
  } deriving (Show, Eq)

instance FromJSON Header where
  parseJSON = withObject "Header" $ \o -> do
    Header <$> o .: "name"
           <*> o .: "value"
  
data QueryItem = QueryItem
  { qName  :: Text
  , qValue :: Text
  } deriving (Show, Eq)
  
instance FromJSON QueryItem where
  parseJSON = withObject "QueryItem" $ \o -> do 
    QueryItem <$> o .: "name"
              <*> o .: "value"
  
data PostData = PostData
  { pMimeType :: Text
  , pText     :: Text
  } deriving (Show, Eq)
  
instance FromJSON PostData where
  parseJSON = withObject "PostData" $ \o -> do
    PostData <$> o .: "mimeType"
             <*> o .: "text"

toHTTP :: UTCTime -> Request -> Either SomeException HTTP.Request
toHTTP now Request {..} = do
  initRequest <- HTTP.parseUrlThrow 
               $ T.unpack
               $ head
               $ T.splitOn "?"
               $ decodeUtf8 
               $ HTTP.urlDecode False 
               $ encodeUtf8 rUrl 
  
  version <- parseHTTPVersion rHttpVersion
  
  httpCookies <- mapM (toHTTPCookie now) rCookies
  
  return $ initRequest 
    { HTTP.method         = encodeUtf8 rMethod 
    , HTTP.queryString    = toHTTPQueryString rQueryString
    , HTTP.requestHeaders = filter ((/= "Content-Length"). fst) $ map toHTTPHeader rHeaders
    , HTTP.requestBody    = HTTP.RequestBodyLBS . BSL.fromStrict $ maybe "" toBody rPostData
    , HTTP.requestVersion = version
    , HTTP.cookieJar      = Just $ HTTP.createCookieJar httpCookies
    }

toBody :: PostData -> ByteString
toBody PostData {..} = encodeUtf8 pText

parseHTTPVersion :: Text -> Either SomeException HTTP.HttpVersion
parseHTTPVersion = \case
  "HTTP/2.0" -> Right $ HTTP.HttpVersion 2 0
  "HTTP/1.1" -> Right $ HTTP.http11
  "HTTP/1.0" -> Right $ HTTP.http10
  "HTTP/0.9" -> Right $ HTTP.http09
  "unknown"  -> Right $ HTTP.http11
  x          -> Left  $ toException $ userError $ "unknown http version " ++ T.unpack x

toHTTPQueryString :: [QueryItem] -> ByteString
toHTTPQueryString = HTTP.urlDecode False . HTTP.renderQuery False . map toQueryItem 

toQueryItem :: QueryItem -> HTTP.QueryItem
toQueryItem QueryItem {..} = (encodeUtf8 qName, Just $ encodeUtf8 qValue)

toHTTPHeader :: Header -> HTTP.Header
toHTTPHeader Header {..} = (CI.mk $ encodeUtf8 hName, encodeUtf8 hValue) 

toHTTPCookie :: UTCTime -> Cookie -> Either SomeException HTTP.Cookie
toHTTPCookie now Cookie {..} = do
  let nullUTCTime = UTCTime (365000 `addDays` utctDay now) (secondsToDiffTime 0)
  expiryTime <- maybe (return nullUTCTime) parseExpiry cExpires 
  return HTTP.Cookie
    { HTTP.cookie_name              = encodeUtf8 cName
    , HTTP.cookie_value             = encodeUtf8 cValue
    , HTTP.cookie_expiry_time       = expiryTime
    , HTTP.cookie_domain            = maybe "" encodeUtf8 cDomain
    , HTTP.cookie_path              = maybe "" encodeUtf8 cPath
    , HTTP.cookie_creation_time     = now
    , HTTP.cookie_last_access_time  = now
    , HTTP.cookie_persistent        = True
    , HTTP.cookie_host_only         = False
    , HTTP.cookie_secure_only       = fromMaybe False cSecure
    , HTTP.cookie_http_only         = fromMaybe False cHttpOnly
    }

makeRequestKey :: Int -> HTTP.Request -> String
makeRequestKey index req = show index ++ " " ++ show (HTTP.getUri req)

harFileToRequests :: FilePath -> IO ([SomeException], [HTTP.Request])
harFileToRequests filePath = do 
  harFile <-  either (throwIO . userError) return 
          =<< eitherDecode <$> BSL.readFile filePath
  now  <- getCurrentTime

  let harRequests = map eRequest $ lEntries $ hLog harFile
  return $ partitionEithers $ map (toHTTP now) harRequests 

recordRequest :: HTTP.Manager -> Recorder -> Int -> HTTP.Request -> IO ()
recordRequest manager recorder index req = do 
  let key = makeRequestKey index req
  record recorder key $ void $ HTTP.httpLbs req manager

-- | 'runHar' takes in a file path to HAR dump created by Chrome's Inspector
--   and creates a function that be used by 'wrecker''s 'run' or 
--   'defaultMain'.
runHar :: FilePath -> IO (Recorder -> IO ())
runHar filePath = do 
  (errors, reqs) <- harFileToRequests filePath
  mapM_ print errors
  
  context <- initConnectionContext
  
  return $ \recorder -> do 
#if MIN_VERSION_http_client(0,5,0)
    manager <- HTTP.newManager 
           $ (mkManagerSettingsContext (Just context) def Nothing)
               { HTTP.managerResponseTimeout = HTTP.responseTimeoutNone }
#else
    manager <- HTTP.newManager 
           $ (mkManagerSettingsContext (Just context) def Nothing)
               { HTTP.managerResponseTimeout = Nothing }
#endif

  
    mapM_ (uncurry $ recordRequest manager recorder) $ zip [0..] reqs
