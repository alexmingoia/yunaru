{-# LANGUAGE OverloadedStrings #-}

module App.Controller.NotFound where

import App.Controller.Page
import App.Model.Env
import Data.Text.Encoding
import Network.Wai
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import Web.Twain

get :: RouteM AppEnv a
get = do
  reqPath <- rawPathInfo <$> request
  sendHtmlPage status404 "Page Not Found" $
    do
      H.h1 $ do
        H.span "Page not found: "
        H.code (toHtml (decodeUtf8 reqPath))
      H.p "You may have a typo in the URL, or the page has since been deleted."

response :: AppEnv -> Response
response env = do
  status status404 $ html
    $ renderPage (appName env) "Page Not Found" "" Nothing
    $ do
      H.h1 "Page not found."
      H.p "You may have a typo in the URL, or the page has since been deleted."
