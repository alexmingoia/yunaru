{-# LANGUAGE OverloadedStrings #-}

module App.Model.Image where

import App.Model.Crypto as Crypto
import App.Model.Env
import qualified Codec.Base64Url.Unpadded as Base64Url
import Data.Text as T
import Data.Text.Encoding

cachedImageMaxAge = 604800

-- Return proxy URL for image URL.
--
-- See: https://docs.imgproxy.net/#/generating_the_url_basic
cacheUrl :: AppEnv -> Int -> Int -> Bool -> URL -> URL
cacheUrl env width height crop url =
  if renderAuthority url == renderAuthority (appImageProxyUrl env)
    then url
    else
      let ext = urlExt url
          resizeType = if crop then "fill" else "fit"
          widthP = T.pack (show width)
          heightP = T.pack (show height)
          gravity = "ce"
          enlarge = "f"
          output = if ext == "gif" || ext == "svg" then ext else "jpg"
          base64url = Base64Url.encode (encodeUtf8 (renderUrl url))
          basePath = [resizeType, widthP, heightP, gravity, enlarge, base64url <> "." <> output]
          hmacMsg = appImageProxySalt env <> encodeUtf8 ("/" <> T.intercalate "/" basePath)
          signature = Base64Url.encode $ Crypto.hmac_sha256 (appImageProxyKey env) hmacMsg
       in appImageProxyUrl env +> (signature : basePath)

urlExt = snd . T.breakOnEnd "." . renderPath
