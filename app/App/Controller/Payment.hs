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
import App.Model.Stripe as Stripe
import App.Model.User as User
import App.View.Payment
import qualified Codec.Base16 as Hex
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.UUID as UUID (toText)
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
  now <- liftIO getCurrentTime
  let updatedUser = user {userPaidAt = Just now}
  DB.exec $ User.save updatedUser
  sessionCookie <- Session.createSessionCookie updatedUser
  send $ withCookie' sessionCookie $ redirect303 (renderUrl userUrl)

catchStripeError :: IO a -> RouteM AppEnv a
catchStripeError = (either (Error.getPlaintext :: StripeError -> RouteM AppEnv a) pure =<<) . liftIO . try
