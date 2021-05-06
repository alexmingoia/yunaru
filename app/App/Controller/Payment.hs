{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Controller.Payment where

import App.Controller.Error as Error
import App.Controller.Session as Session
import App.Model.Crypto as Crypto
import qualified App.Model.Database as DB
import App.Model.Env
import App.Model.Mnemonic as Mnemonic
import App.Model.Stripe as Stripe
import App.Model.User as User
import App.View.Page
import App.View.Payment
import qualified Codec.Base16 as Hex
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as JSON hiding (json)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.List as L
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.UUID as UUID (toText)
import Network.Wai (strictRequestBody)
import Network.Wai.Responder

getPaymentForm :: Responder AppEnv IO a
getPaymentForm = do
  env <- getEnv
  user <- Session.requireUser
  let editUserUrl = appUrl env +> ["users", UUID.toText (userId user), "edit"]
  when (isNothing (userEmail user)) $ send (redirect303 (renderUrl editUserUrl))
  when (userPaid user) $ send (redirect303 (renderUrl (appUrl env)))
  sendHtmlPage status200
    $ withLocation (TitledLocation "Payment")
    $ withHead (stripeJsHtml env)
    $ withHtml
    $ paymentFormHtml

getStripeCheckoutSessionId :: Responder AppEnv IO a
getStripeCheckoutSessionId = do
  env <- getEnv
  user <- Session.requireUser
  let userUrl = appUrl env +> ["users", UUID.toText (userId user), "edit"]
  email <- maybe (send (redirect303 (renderUrl userUrl))) pure (userEmail user)
  if userPaid user
    then send $ redirect302 (renderUrl (appUrl env))
    else do
      let key = encodeUtf8 (appSecret env)
          msg = encodeUtf8 (UUID.toText (userId user) <> ":stripe")
          token = Hex.encode (Crypto.hmac_sha256 key msg)
          sessionCreate =
            StripeCheckoutSessionCreate
              { checkoutCancelUrl = userUrl,
                checkoutSuccessUrl = appUrl env +> ["payments", "stripe", "success", token],
                checkoutCustomerEmail = email,
                checkoutCustomerUserId = userId user,
                checkoutPrice = appStripePriceId env
              }
      (StripeCheckoutSession id) <- catchStripeError $ reqStripe env sessionCreate
      send $ plaintext status200 (BL.fromStrict (encodeUtf8 id))

redirectToStripeCustomerPortal :: Responder AppEnv IO a
redirectToStripeCustomerPortal = do
  env <- getEnv
  user <- Session.requireUser
  let userUrl = appUrl env +> ["users", UUID.toText (userId user), "edit"]
  case userStripeCustomerId user of
    Nothing -> send (redirect303 (renderUrl userUrl))
    Just cid -> do
      let portalSessionCreate = StripeCustomerPortalSessionCreate cid userUrl
      (StripeCustomerPortal url) <- catchStripeError $ reqStripe env portalSessionCreate
      send $ redirect303 (renderUrl url)

verifyStripeCheckoutSuccess :: Text -> Responder AppEnv IO a
verifyStripeCheckoutSuccess tokenP = do
  env <- getEnv
  user <- Session.requireUser
  let userUrl = appUrl env +> ["users", UUID.toText (userId user), "edit"]
  let key = encodeUtf8 (appSecret env)
      msg = encodeUtf8 (UUID.toText (userId user) <> ":stripe")
      token = Hex.encode (Crypto.hmac_sha256 key msg)
  when (token /= tokenP) $ send (redirect303 (renderUrl userUrl))
  newsletterId <- liftIO Mnemonic.nextRandom
  DB.exec $ User.save (user {userStatus = "active", userNewsletterId = Just newsletterId})
  send $ redirect303 (renderUrl userUrl)

catchStripeError :: IO a -> Responder AppEnv IO a
catchStripeError = (either (Error.getPlaintext :: StripeError -> Responder AppEnv IO a) pure =<<) . liftIO . try

data StripeEvent = StripeEventSubscription StripeSubscription

instance FromJSON StripeEvent where
  parseJSON (Object v) = do
    t <- v .: "type"
    d <- v .: "data"
    o <- d .: "object"
    case fst (T.breakOnEnd "." t) of
      "customer.subscription." -> StripeEventSubscription <$> parseJSON o
      prefix -> fail $ T.unpack $ "parsing StripeEvent failed, unsupported event type: " <> prefix
  parseJSON invalid =
    prependFailure
      "parsing StripeEvent failed, "
      (typeMismatch "Object" invalid)

postStripeWebhook :: Responder AppEnv IO a
postStripeWebhook = do
  let invalidHeaderRes = plaintext status400 "invalid signature header"
      missingHeaderRes = plaintext status400 "missing signature header"
  env <- getEnv
  sigH <- maybe (send missingHeaderRes) pure =<< getHeader "Stripe-Signature"
  reqBody <- strictRequestBody <$> getRequest
  reqBodyBS <- BL.toStrict <$> liftIO reqBody
  let sigParts = fmap (T.drop 1) . T.break (== '=') <$> T.splitOn "," sigH
      sigField = "v1"
      secret = encodeUtf8 (appStripeWebhookSecret env)
  ts <- maybe (send invalidHeaderRes) (pure . snd) $ L.find ((== "t") . fst) sigParts
  sig <- maybe (send invalidHeaderRes) (pure . snd) $ L.find ((== sigField) . fst) sigParts
  let payload = encodeUtf8 ts <> "." <> reqBodyBS
      hmac = Hex.encode $ Crypto.hmac_sha256 secret payload
  when (hmac /= sig) $ do
    when (appDebug env) $ do
      liftIO $ do
        print "invalid signature"
        print $ "Computed signature: " <> hmac
        print $ "Stripe webhook signature: " <> sig
    send (plaintext status400 "invalid signature")
  case eitherDecodeStrict reqBodyBS of
    Left e -> do
      liftIO $ putStrLn e
      send (plaintext status204 "")
    Right (StripeEventSubscription s) -> do
      let customerGet = StripeCustomerGet (stripeSubscriptionCustomerId s)
      (c :: StripeCustomer) <- catchStripeError $ reqStripe env customerGet
      DB.exec $ Stripe.updateUserStripeCustomer c s
      send (plaintext status204 "")
