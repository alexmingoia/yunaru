{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Router where

import App.Controller.About as About
import App.Controller.Author as Author
import App.Controller.Entry as Entry
import App.Controller.Feed as Feed
import App.Controller.Following as Following
import App.Controller.MagicLink as MagicLink
import App.Controller.NotFound as NotFound
import App.Controller.Payment as Payment
import App.Controller.Session as Session
import App.Controller.User as User
import App.Model.Env
import Network.Wai.Responder

router :: Responder AppEnv IO ()
router = do
  reqPath <- getPath
  reqMethod <- getMethod
  case reqMethod of
    "GET" -> case reqPath of
      [] -> Entry.getFollowing Nothing
      ["authors", url] -> Author.get url
      ["followings"] -> Following.getRecentEntryList Nothing
      ["magic-links", "new"] -> MagicLink.getForm Nothing
      ["magic-links", "sent"] -> MagicLink.getSentMessage
      ["magic-links", id] -> MagicLink.get id
      ["payments", "new"] -> Payment.getPaymentForm
      ["payments", "stripe", "portal"] -> Payment.redirectToStripeCustomerPortal
      ["payments", "stripe", "success", token] -> Payment.verifyStripeCheckoutSuccess token
      ["privacy"] -> About.getPrivacyPolicy
      ["sessions", "new"] -> Session.getSigninForm Nothing
      ["terms"] -> About.getTerms
      ["users", "new"] -> User.getForm Nothing
      ["users", _, "edit"] -> User.getForm Nothing
      ["feeds", feedUrl] -> Feed.get feedUrl
      ["feeds", feedUrl, "entries", entryUrl] -> Entry.get feedUrl entryUrl
      _ -> NotFound.get
    "POST" -> case reqPath of
      [] -> Following.post
      ["followings"] -> Following.post
      ["magic-links"] -> MagicLink.post
      ["sessions"] -> Session.post
      ["payments", "stripe", "checkout-sessions"] -> Payment.getStripeCheckoutSessionId
      ["users"] -> User.post
      ["webhooks", "stripe"] -> Payment.postStripeWebhook
      _ -> NotFound.get
    "PUT" -> case reqPath of
      ["followings", url] -> Following.put url
      ["users", id] -> User.put id
      _ -> NotFound.get
    "DELETE" -> case reqPath of
      ["followings", url] -> Following.delete url
      ["sessions", _] -> Session.delete
      _ -> NotFound.get
    _ -> NotFound.get
