{-# LANGUAGE OverloadedStrings #-}

module App.Controller.NotFound where

import App.Model.Env
import App.View.Page
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Responder
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H

get :: Responder AppEnv IO a
get = do
  reqPath <- getRawPath
  sendHtmlPage status404
    $ withLocation (TitledLocation "Page Not Found")
    $ withHtml
    $ do
      H.h1 $ do
        H.span "Page not found: "
        H.code (toHtml (decodeUtf8 reqPath))
      H.p "You may have a typo in the URL, or the page has since been deleted."

response :: AppEnv -> Response
response env = do
  html status404
    $ renderPage env Nothing
    $ withLocation (TitledLocation "Page Not Found")
    $ withHtml
    $ do
      H.h1 "Page not found."
      H.p "You may have a typo in the URL, or the page has since been deleted."
