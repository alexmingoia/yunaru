{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Error where

import App.Controller.Page
import App.Model.Env
import Control.Exception
import Control.Monad
import Data.Text
import Network.Wai
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import Web.Twain

-- | Respond with an error page.
get :: (Exception e) => e -> RouteM AppEnv a
get err = do
  let msg = pack (displayException err)
  sendHtmlPage (errorStatus (Just err)) "Error" $
    H.h1 (toHtml msg)

getPlaintext :: (Exception e) => e -> RouteM AppEnv a
getPlaintext err = do
  let msg = pack (displayException err)
  send $ status (errorStatus (Just err)) $ text msg

-- | Construct a Response from exception.
response :: AppEnv -> SomeException -> Response
response env err =
  let msg = pack (displayException (toAppError err))
   in status (errorStatus (Just err))
        $ html
        $ renderPage (appName env) "Error" "" Nothing
        $ H.h1 (toHtml msg)

ignore :: AppEnv -> SomeException -> IO ()
ignore env e = when (appDebug env) (putStrLn (displayException e))
