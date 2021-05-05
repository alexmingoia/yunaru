{-# LANGUAGE OverloadedStrings #-}

module App.WebServer where

import qualified App.Controller.Error as Error
import App.Controller.Router (router)
import qualified App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.Env
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.List as L
import Data.Maybe
import Data.Set as Set
import Data.Word8 (_semicolon)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Throttle
import Network.Wai.Middleware.Vhost
import Network.Wai.Responder
import System.Clock (TimeSpec (..))
import System.Environment

serve :: IO ()
serve = do
  env <- getAppEnv
  port <- maybe (fromMaybe 2020 (urlPort (appUrl env))) read <$> lookupEnv "PORT"
  let serverSettings = setPort port (setOnExceptionResponse (Error.response env) defaultSettings)
  connEnv <- DB.withPool env
  runSettings serverSettings =<< middleware connEnv (app connEnv)
  where
    app :: AppEnv -> Application
    app env req respond = do
      let maxReqSize = 5000000
      parsedReq <- parseRequest maxReqSize req
      sessEnv <- Session.withUser env req
      eres <- runResponder sessEnv parsedReq router
      either respond (const noResError) eres
    noResError :: IO a
    noResError = do
      putStrLn "Failed to construct a response."
      throwIO (InternalError "Failed to construct a response.")

middleware env app = do
  let noCachePolicy = CustomCaching (const [("Cache-Control", "no-cache")])
      defThSettings = defaultThrottleSettings (TimeSpec 5 0)
      thSettings = defThSettings {throttleSettingsRate = 2, throttleSettingsPeriod = 1, throttleSettingsBurst = 10}
      gzipSettings = def {gzipCheckMime = gzippable}
      logRequests = if appDebug env then logStdoutDev else id
  th <- initCustomThrottler thSettings throttleKey
  cacheContainer <- if appProduction env then initCaching PublicStaticCaching else initCaching noCachePolicy
  return
    $ throttle th
    $ redirectWWW (renderUrl (appUrl env))
    $ autohead
    $ methodOverride
    $ gzip gzipSettings
    $ logRequests
    $ staticPolicy' cacheContainer (hasPrefix "assets")
    $ app

-- Get the key used for rate-limiter.
throttleKey :: Request -> Either Response Char8.ByteString
throttleKey r =
  let xf = snd <$> L.find (\(n, _) -> n == "X-Forwarded-For" || n == "x-forwarded-for") (requestHeaders r)
      rh = Char8.pack (show (remoteHost r))
   in Right (fromMaybe rh xf)

-- Check if mime-type should be gzipped.
gzippable :: BS.ByteString -> Bool
gzippable bs = Char8.isPrefixOf "text/" bs || Set.member bs' mimetypes
  where
    bs' = fst $ BS.break (== _semicolon) bs
    mimetypes =
      Set.fromList
        [ "application/json",
          "application/javascript",
          "application/rss+xml",
          "image/x-icon"
        ]
