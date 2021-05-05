{-# LANGUAGE ScopedTypeVariables #-}

module App.Model.Env
  ( module App.Model.Env,
    module App.Model.URL,
    module App.Model.User,
    module App.Model.Error,
    module App.Model.EmailAddress,
  )
where

import App.Model.EmailAddress
import App.Model.Error
import App.Model.URL
import App.Model.User
import qualified Codec.Base16 as Hex
import Control.Exception
import qualified Data.ByteString as B
import Data.Either.Combinators
import Data.Pool
import Data.Text
import Data.Text.Encoding
import Database.Selda.Backend
import Database.Selda.PostgreSQL
import Network.AWS as AWS
import Network.AWS.Data.Text as AWSData
import System.Environment

data AppEnv
  = AppEnv
      { appDebug :: Bool,
        appProduction :: Bool,
        appName :: Text,
        appEmail :: Email,
        appUrl :: URL,
        appSecret :: Text,
        appDbConn :: Maybe (Either AppDBConn (Pool AppDBConn)),
        appUser :: Maybe User,
        appDbName :: Text,
        appDbHost :: Text,
        appDbUser :: Text,
        appDbPass :: Text,
        appAwsSesKey :: AWS.AccessKey,
        appAwsSesSecret :: AWS.SecretKey,
        appAwsSesRegion :: AWS.Region,
        appImageProxyUrl :: URL,
        appImageProxyKey :: B.ByteString,
        appImageProxySalt :: B.ByteString,
        appTwConsumerKey :: Text,
        appTwConsumerSecret :: Text,
        appStripePublicKey :: Text,
        appStripeSecretKey :: Text,
        appStripePriceId :: Text,
        appStripeWebhookSecret :: Text
      }

type AppDBConn = SeldaConnection PG

-- `fromJust` is used here so app exits if env variables are missing.
getAppEnv :: IO AppEnv
getAppEnv = do
  isDebug <- maybe False (/= "") <$> lookupEnv "DEBUG"
  isProduction <- maybe False (== "production") <$> lookupEnv "APP_ENV"
  appName <- getEnvText "APP_NAME"
  appUrl <- getEnvUrl "APP_URL"
  appEmail <- getEnvEmail "APP_EMAIL"
  appSecret <- getEnvText "APP_SECRET"
  appDbName <- getEnvText "APP_DB_NAME"
  appDbHost <- getEnvText "APP_DB_HOST"
  appDbUser <- getEnvText "APP_DB_USER"
  appDbPass <- getEnvText "APP_DB_PASS"
  appAwsSesKey <- AccessKey . encodeUtf8 <$> getEnvText "AWS_SES_KEY"
  appAwsSesSecret <- SecretKey . encodeUtf8 <$> getEnvText "AWS_SES_SECRET"
  sesRegionText <- getEnvText "AWS_SES_REGION"
  appAwsSesRegion <- maybe (throwIO (regionError sesRegionText)) pure (rightToMaybe (AWSData.fromText sesRegionText))
  appImageProxyUrl <- getEnvUrl "APP_IMAGE_PROXY_URL"
  appImageProxyKey <- getEnvHex "APP_IMAGE_PROXY_KEY"
  appImageProxySalt <- getEnvHex "APP_IMAGE_PROXY_SALT"
  appTwConsumerKey <- getEnvText "TW_CONSUMER_KEY"
  appTwConsumerSecret <- getEnvText "TW_CONSUMER_SECRET"
  appStripePublicKey <- getEnvText "APP_STRIPE_PUBLIC_KEY"
  appStripeSecretKey <- getEnvText "APP_STRIPE_SECRET_KEY"
  appStripePriceId <- getEnvText "APP_STRIPE_PRICE_ID"
  appStripeWebhookSecret <- getEnvText "APP_STRIPE_WEBHOOK_SECRET"
  return $
    AppEnv
      { appDebug = isDebug,
        appProduction = isProduction,
        appUrl = appUrl,
        appEmail = appEmail,
        appSecret = appSecret,
        appName = appName,
        appDbConn = Nothing,
        appUser = Nothing,
        appDbName = appDbName,
        appDbHost = appDbHost,
        appDbUser = appDbUser,
        appDbPass = appDbPass,
        appAwsSesKey = appAwsSesKey,
        appAwsSesSecret = appAwsSesSecret,
        appAwsSesRegion = appAwsSesRegion,
        appImageProxyUrl = appImageProxyUrl,
        appImageProxyKey = appImageProxyKey,
        appImageProxySalt = appImageProxySalt,
        appTwConsumerKey = appTwConsumerKey,
        appTwConsumerSecret = appTwConsumerSecret,
        appStripePublicKey = appStripePublicKey,
        appStripeSecretKey = appStripeSecretKey,
        appStripePriceId = appStripePriceId,
        appStripeWebhookSecret = appStripeWebhookSecret
      }

regionError label = userError ("Invalid AWS region: " <> unpack label)

urlError name = userError ("Failed to parse URL for " <> name)

emailError name = userError ("Failed to parse email for " <> name)

getEnvUrl :: String -> IO URL
getEnvUrl name =
  maybe (throwIO (urlError name)) pure =<< (parseAbsoluteUrl <$> getEnvText name)

getEnvEmail :: String -> IO Email
getEnvEmail name =
  maybe (throwIO (urlError name)) pure =<< (parseEmail <$> getEnvText name)

getEnvText :: String -> IO Text
getEnvText name = pack <$> getEnv name

getEnvHex :: String -> IO B.ByteString
getEnvHex name = either (throwIO . userError) pure =<< (Hex.decode <$> getEnvText name)
