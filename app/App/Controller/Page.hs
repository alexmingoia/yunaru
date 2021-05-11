module App.Controller.Page
  ( sendHtmlPage,
    module App.View.Page,
  )
where

import App.Model.Crypto
import App.Model.Database as DB
import App.Model.Env
import App.Model.User as User
import App.View.Page
import Data.Text.Encoding
import Data.UUID as UUID
import Network.Wai (rawPathInfo)
import Text.Blaze.Html (Html)
import Web.Twain

sendHtmlPage :: Status -> Text -> Html -> RouteM AppEnv a
sendHtmlPage s title markup = do
  reqPath <- decodeUtf8 . rawPathInfo <$> request
  secret <- appSecret <$> env
  sidM <- paramMaybe "sid"
  let userIdM = UUID.fromText =<< decrypt secret =<< sidM
  userM <- maybe (pure Nothing) (DB.exec . User.findOne) userIdM
  appName <- appName <$> env
  send $ status s $ html $ renderPage appName title reqPath userM markup
