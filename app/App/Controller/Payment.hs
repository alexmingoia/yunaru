{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Controller.Payment where

import App.Controller.Error as Error
import App.Controller.NotFound as NotFound
import App.Controller.Page
import App.Controller.Session as Session
import App.Model.Crypto as Crypto
import qualified App.Model.Database as DB
import App.Model.Env
import App.Model.Mnemonic as Mnemonic
import App.Model.Stripe as Stripe
import App.Model.User as User
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
import Web.Twain

getPaymentForm :: RouteM AppEnv a
getPaymentForm = do
  appEnv <- env
  user <- Session.requireUser
  let editUserUrl = rootUrl +> ["users", UUID.toText (userId user), "edit"]
  when (isNothing (userEmail user)) $ send $ redirect303 (renderUrl editUserUrl)
  when (userPaid user) $ send $ redirect303 "/"
  sendHtmlPage status200 "Payment" $ paymentFormHtml appEnv

getStripeCheckoutSessionId :: RouteM AppEnv a
getStripeCheckoutSessionId = do
  e <- env
  u <- Session.requireUser
  user <- maybe NotFound.get pure =<< DB.exec (User.findOne (userId u))
  let userUrl = appUrl e +> ["users", UUID.toText (userId user), "edit"]
  email <- param "email"
  if userPaid user
    then send $ redirect302 (renderUrl (appUrl e))
    else do
      let key = encodeUtf8 (appSecret e)
          msg = encodeUtf8 (UUID.toText (userId user) <> ":stripe")
          token = Hex.encode (Crypto.hmac_sha256 key msg)
          sessionCreate =
            StripeCheckoutSessionCreate
              { checkoutCancelUrl = userUrl,
                checkoutSuccessUrl = appUrl e +> ["payments", "stripe", "success", token],
                checkoutCustomerEmail = email,
                checkoutCustomerUserId = userId user,
                checkoutPrice = appStripePriceId e
              }
      (StripeCheckoutSession id) <- catchStripeError $ reqStripe e sessionCreate
      send $ text id

redirectToStripeCustomerPortal :: RouteM AppEnv a
redirectToStripeCustomerPortal = do
  e <- env
  user <- Session.requireUser
  let userUrl = appUrl e +> ["users", UUID.toText (userId user), "edit"]
  case userStripeCustomerId user of
    Nothing -> send (redirect303 (renderUrl userUrl))
    Just cid -> do
      let portalSessionCreate = StripeCustomerPortalSessionCreate cid userUrl
      (StripeCustomerPortal url) <- catchStripeError $ reqStripe e portalSessionCreate
      send $ redirect303 (renderUrl url)

verifyStripeCheckoutSuccess :: RouteM AppEnv a
verifyStripeCheckoutSuccess = do
  e <- env
  user <- Session.requireUser
  tokenP <- param "token" :: RouteM AppEnv Text
  let userUrl = appUrl e +> ["users", UUID.toText (userId user), "edit"]
  let key = encodeUtf8 (appSecret e)
      msg = encodeUtf8 (UUID.toText (userId user) <> ":stripe")
      token = Hex.encode (Crypto.hmac_sha256 key msg)
  when (token /= tokenP) $ send (redirect303 (renderUrl userUrl))
  newsletterId <- liftIO Mnemonic.nextRandom
  let updatedUser = user {userStatus = "active", userNewsletterId = Just newsletterId}
  DB.exec $ User.save updatedUser
  sessionCookie <- Session.createSessionCookie updatedUser
  send $ withCookie' sessionCookie $ redirect303 (renderUrl userUrl)

catchStripeError :: IO a -> RouteM AppEnv a
catchStripeError = (either (Error.getPlaintext :: StripeError -> RouteM AppEnv a) pure =<<) . liftIO . try

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

postStripeWebhook :: RouteM AppEnv a
postStripeWebhook = do
  let invalidHeaderRes = status status400 $ text "invalid signature header"
      missingHeaderRes = status status400 $ text "missing signature header"
  e <- env
  sigH <- maybe (send missingHeaderRes) pure =<< header "Stripe-Signature"
  reqBody <- strictRequestBody <$> request
  reqBodyBS <- BL.toStrict <$> liftIO reqBody
  let sigParts = fmap (T.drop 1) . T.break (== '=') <$> T.splitOn "," sigH
      sigField = "v1"
      secret = encodeUtf8 (appStripeWebhookSecret e)
  ts <- maybe (send invalidHeaderRes) (pure . snd) $ L.find ((== "t") . fst) sigParts
  sig <- maybe (send invalidHeaderRes) (pure . snd) $ L.find ((== sigField) . fst) sigParts
  let payload = encodeUtf8 ts <> "." <> reqBodyBS
      hmac = Hex.encode $ Crypto.hmac_sha256 secret payload
  when (hmac /= sig) $ do
    when (appDebug e) $ do
      liftIO $ do
        print "invalid signature"
        print $ "Computed signature: " <> hmac
        print $ "Stripe webhook signature: " <> sig
    send $ status status400 $ text "invalid signature"
  case eitherDecodeStrict reqBodyBS of
    Left e -> do
      liftIO $ putStrLn e
      send $ status status204 $ text ""
    Right (StripeEventSubscription s) -> do
      let customerGet = StripeCustomerGet (stripeSubscriptionCustomerId s)
      (c :: StripeCustomer) <- catchStripeError $ reqStripe e customerGet
      DB.exec $ Stripe.updateUserStripeCustomer c s
      send $ status status204 $ text ""
