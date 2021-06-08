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
import Data.Aeson as JSON
import Data.Text.Encoding
import Data.UUID as UUID
import Network.Wai (rawPathInfo)
import Text.Blaze.Html (Html)
import Web.Twain

sendHtmlPage :: Status -> Text -> Html -> RouteM AppEnv a
sendHtmlPage s title markup = do
  reqPath <- decodeUtf8 . rawPathInfo <$> request
  appName <- appName <$> env
  userM <- getUser
  returnMinimal <- maybe False (== "return-minimal") <$> header "Prefer"
  if returnMinimal
    then send $ status s $ html $ renderBody' appName reqPath userM markup
    else send $ status s $ html $ renderPage appName title reqPath userM markup

getUser :: RouteM AppEnv (Maybe User)
getUser = do
  secret <- appSecret <$> env
  sessionM <- paramMaybe "session"
  case sessionM of
    Nothing -> do
      sidM <- paramMaybe "sid"
      let userIdM = UUID.fromText =<< decryptHex secret =<< sidM
      maybe (pure Nothing) (DB.exec . User.findOne) userIdM
    Just encryptedJson -> do
      return (decodeStrict =<< decrypt secret encryptedJson)
