{-# LANGUAGE ScopedTypeVariables #-}

module App.Model.Env
  ( module App.Model.Env,
    module App.Model.URL,
    module App.Model.Error,
    module App.Model.EmailAddress,
  )
where

import App.Model.EmailAddress
import App.Model.Error
import App.Model.URL
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
        appDbName :: Text,
        appDbHost :: Text,
        appDbUser :: Text,
        appDbPass :: Text,
        appNewsletterWebhookSecret :: Text,
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
readAppEnv :: IO AppEnv
readAppEnv = do
  isDebug <- maybe False (/= "") <$> lookupEnv "DEBUG"
  isProduction <- maybe False (== "production") <$> lookupEnv "APP_ENV"
  appName <- envText "APP_NAME"
  appUrl <- envUrl "APP_URL"
  appEmail <- envEmail "APP_EMAIL"
  appSecret <- envText "APP_SECRET"
  appDbName <- envText "APP_DB_NAME"
  appDbHost <- envText "APP_DB_HOST"
  appDbUser <- envText "APP_DB_USER"
  appDbPass <- envText "APP_DB_PASS"
  appNewsletterWebhookSecret <- envText "APP_NEWSLETTER_SECRET"
  appAwsSesKey <- AccessKey . encodeUtf8 <$> envText "AWS_SES_KEY"
  appAwsSesSecret <- SecretKey . encodeUtf8 <$> envText "AWS_SES_SECRET"
  sesRegionText <- envText "AWS_SES_REGION"
  appAwsSesRegion <- maybe (throwIO (regionError sesRegionText)) pure (rightToMaybe (AWSData.fromText sesRegionText))
  appImageProxyUrl <- envUrl "APP_IMAGE_PROXY_URL"
  appImageProxyKey <- envHex "APP_IMAGE_PROXY_KEY"
  appImageProxySalt <- envHex "APP_IMAGE_PROXY_SALT"
  appTwConsumerKey <- envText "TW_CONSUMER_KEY"
  appTwConsumerSecret <- envText "TW_CONSUMER_SECRET"
  appStripePublicKey <- envText "APP_STRIPE_PUBLIC_KEY"
  appStripeSecretKey <- envText "APP_STRIPE_SECRET_KEY"
  appStripePriceId <- envText "APP_STRIPE_PRICE_ID"
  appStripeWebhookSecret <- envText "APP_STRIPE_WEBHOOK_SECRET"
  return $
    AppEnv
      { appDebug = isDebug,
        appProduction = isProduction,
        appUrl = appUrl,
        appEmail = appEmail,
        appSecret = appSecret,
        appName = appName,
        appDbConn = Nothing,
        appDbName = appDbName,
        appDbHost = appDbHost,
        appDbUser = appDbUser,
        appDbPass = appDbPass,
        appNewsletterWebhookSecret = appNewsletterWebhookSecret,
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

envUrl :: String -> IO URL
envUrl name =
  maybe (throwIO (urlError name)) pure =<< (parseAbsoluteUrl <$> envText name)

envEmail :: String -> IO Email
envEmail name =
  maybe (throwIO (urlError name)) pure =<< (parseEmail <$> envText name)

envText :: String -> IO Text
envText name = pack <$> getEnv name

envHex :: String -> IO B.ByteString
envHex name = either (throwIO . userError) pure =<< (Hex.decode <$> envText name)
