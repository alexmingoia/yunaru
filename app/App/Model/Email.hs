{-# LANGUAGE OverloadedStrings #-}

module App.Model.Email where

import App.Model.Env
import App.Model.MagicLink
import App.View.MagicLink
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text
import Data.UUID as UUID
import qualified Network.AWS as AWS
import Network.AWS as AWS hiding (send)
import Network.AWS.SES as SES
import System.IO

sendMagicLink :: AppEnv -> MagicLink -> IO ()
sendMagicLink env m = do
  let key = appAwsSesKey env
      secret = appAwsSesSecret env
      region = appAwsSesRegion env
      href = appUrl env +> ["magic-links", UUID.toText (magicLinkId m)]
  when (appDebug env) $ do
    putStrLn $ unpack $ "Sending magic link " <> renderUrl href <> " to " <> renderEmail (magicLinkEmail m)
  when (appProduction env) $ do
    awsEnv <- newEnv $ FromKeys key secret
    lgr <- newLogger (if appDebug env then Debug else Error) stdout
    let appDomain = fromMaybe "localhost" (renderAuthority (appUrl env))
        from = appName env <> " <robot@" <> appDomain <> ">"
        to = set dToAddresses [renderEmail (magicLinkEmail m)] destination
        msg = magicLinkEmailMessage env m
    sendEmailResponse <-
      AWS.runResourceT $ AWS.runAWS (awsEnv & AWS.envLogger .~ lgr)
        $ AWS.within region
        $ AWS.send
        $ SES.sendEmail from to msg
    when (appDebug env) $ print sendEmailResponse
