{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Error where

import App.Model.Env
import App.View.Error
import App.View.Page
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Text
import Data.Text.Encoding
import Network.Wai
import Network.Wai.Responder

-- | Respond with an error page.
get :: (Exception e) => e -> Responder AppEnv IO a
get e = do
  let msg = pack (displayException e)
      status = errorStatus (Just e)
      page = errorPage msg
  sendHtmlPage status page

getPlaintext :: (Exception e) => e -> Responder AppEnv IO a
getPlaintext e = do
  let msg = pack (displayException e)
      status = errorStatus (Just e)
  send $ plaintext status (BL.fromStrict (encodeUtf8 msg))

-- | Construct a Response from exception.
response :: AppEnv -> SomeException -> Response
response env e =
  let msg = pack (displayException (toAppError e))
      status = errorStatus (Just e)
      html = renderPage env Nothing (errorPage msg)
   in responseLBS status [("Content-Type", "text/html; charset=utf-8")] html

ignore :: AppEnv -> SomeException -> IO ()
ignore env e = when (appDebug env) (putStrLn (displayException e))
