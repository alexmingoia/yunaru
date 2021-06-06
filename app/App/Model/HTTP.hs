{-# LANGUAGE OverloadedStrings #-}

module App.Model.HTTP where

import App.Model.Error
import App.Model.URL
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client hiding (parseUrl)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.TLS hiding (Header)

-- | Fetch response body from URL, returning final URL after following redirects.
--
-- We need the final URL so we can use the correct URL in extracted author.
fetchUrl :: URL -> [Header] -> IO (Response BodyReader, BL.ByteString, URL)
fetchUrl url hs = handle (handleTlsError url) $ do
  manager <- newTlsManagerWith (tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 5000000})
  request <- parseRequest (T.unpack (renderUrl url))
  let reqWithHeaders = request {requestHeaders = hs}
  handleResponse url =<< try (httpLbsWithFinalResponse reqWithHeaders manager)
  where
    urlFromReq :: Request -> URL
    urlFromReq req =
      let protocol = if secure req then "https://" else "http://"
       in fromJust . parseAbsoluteUrl . decodeUtf8 $ protocol <> host req <> path req <> queryString req
    httpLbsWithFinalResponse :: Request -> Manager -> IO (Response BodyReader, BL.ByteString, URL)
    httpLbsWithFinalResponse req man = withResponseHistory req man $ \res -> do
      let finalRes = hrFinalResponse res
      bss <- brConsume (responseBody finalRes)
      return (finalRes, BL.fromChunks bss, urlFromReq (hrFinalRequest res))

handleTlsError :: URL -> TLSException -> IO a
handleTlsError url _ = throwIO (ImportError url NoResponse)

-- | Handle HTTP response, translating HTTP exceptions into application errors and retrying requests as necessary.
handleResponse :: URL -> Either HttpException (Response BodyReader, BL.ByteString, URL) -> IO (Response BodyReader, BL.ByteString, URL)
handleResponse url response =
  case response of
    Left (HttpExceptionRequest _ ResponseTimeout) ->
      throwIO (ImportError url NoResponse)
    Left (HttpExceptionRequest _ e) ->
      -- Try http if https fails to connect.
      if isConnectionFailure e && "https://" `T.isPrefixOf` renderUrl url
        then fetchUrl (withHttp url) []
        else throwIO (ImportError url NoResponse)
    Left _ ->
      throwIO (ImportError url NoResponse)
    Right res -> return res
  where
    isConnectionFailure (InternalException _) = True
    isConnectionFailure (ConnectionFailure _) = True
    isConnectionFailure _ = False

data ContentType
  = HTMLContentType
  | XMLContentType
  | UnsupportedContentType
  deriving (Eq, Show)

contentType :: Response body -> ContentType
contentType res =
  let hs = responseHeaders res
      val = maybe "" decodeUtf8 $ listToMaybe $ fmap snd $ filter (\h -> fst h == hContentType) hs
      ct = fromMaybe "" $ listToMaybe $ T.splitOn ";" val
      htmlTypes = ["text/html", "application/xhtml+xml"]
      xmlTypes =
        [ "application/rss+xml",
          "application/atom+xml",
          "application/xml",
          "text/xml",
          "text/plain"
        ]
   in if ct `elem` htmlTypes
        then HTMLContentType
        else if ct `elem` xmlTypes then XMLContentType else UnsupportedContentType

extractETag :: Response body -> Maybe T.Text
extractETag res =
  decodeUtf8 . snd <$> L.find ((== hETag) . fst) (responseHeaders res)
