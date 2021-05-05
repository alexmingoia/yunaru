{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.Stripe where

import App.Model.Env
import App.Model.Selda
import Control.Exception
import Control.Monad
import Control.Monad.Fail
import Data.Aeson as JSON
import Data.Aeson.Types
import qualified Data.ByteString as B
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.UUID as UUID
import Network.HTTP.Client hiding (parseUrl)
import Network.HTTP.Client.TLS
import Network.HTTP.Types

stripeApiUrl = "https://api.stripe.com/v1"

data StripeError = StripeError Text
  deriving (Show)

instance Exception StripeError where
  displayException (StripeError t) = unpack t

instance FromJSON StripeError where
  parseJSON (Object v) = do
    e <- v .: "error"
    msg <- e .: "message"
    return $ StripeError msg
  parseJSON invalid =
    prependFailure
      "parsing StripeError failed, "
      (typeMismatch "Object" invalid)

class StripeApiRequest a where
  stripeApiMethod :: a -> B.ByteString
  stripeApiPath :: a -> Text
  stripeApiBody :: a -> B.ByteString

data StripeSubscription
  = StripeSubscription
      { stripeSubscriptionId :: Text,
        stripeSubscriptionCustomerId :: Text,
        stripeSubscriptionStatus :: Text,
        stripeSubscriptionCurrentPeriodStart :: UTCTime,
        stripeSubscriptionCurrentPeriodEnd :: UTCTime,
        stripeSubscriptionCanceledAt :: Maybe UTCTime,
        stripeSubscriptionCreatedAt :: UTCTime
      }
  deriving (Show)

data StripeCustomerGet = StripeCustomerGet Text

data StripeCustomer
  = StripeCustomer
      { stripeCustomerId :: Text,
        stripeCustomerEmail :: Maybe Email,
        stripeCustomerUserId :: Maybe UUID
      }
  deriving (Show)

instance StripeApiRequest StripeCustomerGet where
  stripeApiMethod _ = "GET"
  stripeApiPath (StripeCustomerGet id) = "/customers/" <> id
  stripeApiBody _ = ""

instance FromJSON StripeCustomer where
  parseJSON (Object v) = do
    id <- v .: "id"
    email <- (parseEmail =<<) <$> v .: "email"
    metadata <- v .: "metadata"
    userId <- (UUID.fromText =<<) <$> metadata .:? "user_id"
    return $ StripeCustomer id email userId
  parseJSON invalid =
    prependFailure
      "parsing StripeCustomer failed, "
      (typeMismatch "Object" invalid)

instance FromJSON StripeSubscription where
  parseJSON (Object v) = do
    id <- v .: "id"
    customerId <- v .: "customer"
    status <- v .: "status"
    currentPeriodStart <- parseJSTime =<< v .: "current_period_start"
    currentPeriodEnd <- parseJSTime =<< v .: "current_period_end"
    canceledAt <- (parseJSTime =<<) <$> v .:? "canceled_at"
    createdAt <- parseJSTime =<< v .: "created"
    return $
      StripeSubscription
        id
        customerId
        status
        currentPeriodStart
        currentPeriodEnd
        canceledAt
        createdAt
  parseJSON invalid =
    prependFailure
      "parsing StripeSubscription failed, "
      (typeMismatch "Object" invalid)

data StripeCustomerPortal = StripeCustomerPortal URL

instance FromJSON StripeCustomerPortal where
  parseJSON (Object v) = do
    urlM <- (parseUrl =<<) <$> v .: "url"
    url <- maybe (Control.Monad.Fail.fail "invalid url") pure urlM
    return $ StripeCustomerPortal url
  parseJSON invalid =
    prependFailure
      "parsing StripeCustomerPortal failed, "
      (typeMismatch "Object" invalid)

data StripeCustomerPortalSessionCreate
  = StripeCustomerPortalSessionCreate
      { stripeCustomerPortalSessionCreateCustomer :: Text,
        stripeCustomerPortalSessionCreateReturnUrl :: URL
      }
  deriving (Show)

instance StripeApiRequest StripeCustomerPortalSessionCreate where
  stripeApiMethod _ = "POST"
  stripeApiPath _ = "/billing_portal/sessions"
  stripeApiBody (StripeCustomerPortalSessionCreate cid url) =
    encodeUtf8 $
      "customer="
        <> cid
        <> "&return_url="
        <> renderUrl url

data StripeCheckoutSessionCreate
  = StripeCheckoutSessionCreate
      { checkoutCancelUrl :: URL,
        checkoutSuccessUrl :: URL,
        checkoutCustomerEmail :: Email,
        checkoutCustomerUserId :: UUID,
        checkoutPrice :: Text
      }

instance StripeApiRequest StripeCheckoutSessionCreate where
  stripeApiMethod _ = "POST"
  stripeApiPath _ = "/checkout/sessions"
  stripeApiBody s =
    "cancel_url="
      <> encodeUtf8 (renderUrl (checkoutCancelUrl s))
      <> "&mode=subscription"
      <> "&payment_method_types[0]=card"
      <> "&success_url="
      <> encodeUtf8 (renderUrl (checkoutSuccessUrl s))
      <> "&customer_email="
      <> urlEncode True (encodeUtf8 (renderEmail (checkoutCustomerEmail s)))
      <> "&client_reference_id="
      <> encodeUtf8 (UUID.toText (checkoutCustomerUserId s))
      <> "&line_items[0][quantity]=1"
      <> "&line_items[0][price]="
      <> encodeUtf8 (checkoutPrice s)

data StripeCheckoutSession = StripeCheckoutSession Text

instance FromJSON StripeCheckoutSession where
  parseJSON (Object v) = do
    id <- v .: "id"
    return $ StripeCheckoutSession id
  parseJSON invalid =
    prependFailure
      "parsing StripeCheckoutSession failed, "
      (typeMismatch "Object" invalid)

updateUserStripeCustomer :: StripeCustomer -> StripeSubscription -> SeldaT PG IO ()
updateUserStripeCustomer c s = do
  let email = lowercaseEmail <$> stripeCustomerEmail c
  update_
    users
    (\u -> u ! #userEmail .== literal email .|| u ! #userStripeCustomerId .== literal (Just (stripeCustomerId c)))
    ( \u ->
        u
          `with` [ #userStripeCustomerId := literal (Just (stripeCustomerId c)),
                   #userStatus := literal (stripeSubscriptionStatus s),
                   #userPaidUntil := literal (Just (stripeSubscriptionCurrentPeriodEnd s)),
                   #userCanceledAt := literal (stripeSubscriptionCanceledAt s)
                 ]
    )

parseJSTime :: (MonadFail m) => Int -> m UTCTime
parseJSTime n = parseTimeM False defaultTimeLocale "%s" (show n)

reqStripe :: (StripeApiRequest a, FromJSON o) => AppEnv -> a -> IO o
reqStripe env action = do
  let hs = [(hAuthorization, "Bearer " <> encodeUtf8 (appStripeSecretKey env))]
  manager <- newManager tlsManagerSettings
  req' <- parseRequest (unpack (stripeApiUrl <> stripeApiPath action))
  let req =
        req'
          { method = stripeApiMethod action,
            requestHeaders = hs,
            requestBody = RequestBodyBS (stripeApiBody action)
          }
  res <- httpLbs req manager
  when (appDebug env) $ print (responseBody res)
  if responseStatus res >= status200 && responseStatus res < status300
    then either (throwIO . StripeError . pack) pure (JSON.eitherDecode (responseBody res))
    else
      either
        (throwIO . StripeError . pack)
        throwIO
        (JSON.eitherDecode (responseBody res) :: Either String StripeError)
