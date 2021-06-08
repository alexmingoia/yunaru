{-# LANGUAGE OverloadedStrings #-}

module App.WebServer where

import qualified App.Controller.About as About
import qualified App.Controller.Author as Author
import qualified App.Controller.Discover as Discover
import qualified App.Controller.Entry as Entry
import qualified App.Controller.Error as Error
import qualified App.Controller.Feed as Feed
import qualified App.Controller.Following as Following
import qualified App.Controller.MagicLink as MagicLink
import qualified App.Controller.Newsletter as Newsletter
import qualified App.Controller.NotFound as NotFound
import qualified App.Controller.Payment as Payment
import qualified App.Controller.Session as Session
import qualified App.Controller.User as User
import qualified App.Model.Database as DB
import App.Model.Env as Env
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Twain

serve :: IO ()
serve = do
  env <- DB.withPool =<< Env.readAppEnv
  cacheContainer <- initCacheContainer env
  twain 2020 env $ do
    middleware $ autohead
    middleware $ methodOverride
    middleware $ if appDebug env then logStdoutDev else id
    middleware $ staticPolicy' cacheContainer (hasPrefix "assets")
    get "/" $ Entry.getFollowing Nothing
    get "/authors/:url" Author.get
    get "/discover" $ Discover.list
    get "/followings" $ Following.getRecentEntryList Nothing
    get "/magic-links/new" $ MagicLink.getForm Nothing
    get "/magic-links/sent" MagicLink.getSentMessage
    get "/magic-links/:id" MagicLink.get
    get "/payments/new" Payment.getPaymentForm
    get "/payments/stripe/success/:token" Payment.verifyStripeCheckoutSuccess
    get "/privacy" About.getPrivacyPolicy
    get "/sessions/new" $ Session.getSigninForm Nothing
    get "/terms" About.getTerms
    get "/users/new" $ User.getForm Nothing
    get "/users/:id/edit" $ User.getForm Nothing
    get "/feeds/:url" Feed.get
    get "/feeds/:feedUrl/entries/:entryUrl" Entry.get
    post "/" Following.post
    post "/followings" Following.post
    post "/magic-links" MagicLink.post
    post "/sessions" Session.post
    post "/payments/stripe/checkout-sessions" Payment.getStripeCheckoutSessionId
    post "/users" User.post
    post "/webhooks/newsletters" Newsletter.postNewsletterWebhook
    put "/followings/:url" Following.put
    put "/users/:id" User.put
    delete "/followings/:url" Following.delete
    delete "/sessions/:id" Session.delete
    notFound NotFound.get
    onException $ Error.response env

initCacheContainer env = do
  let noCachePolicy = CustomCaching (const [("Cache-Control", "no-cache")])
  if appProduction env
    then initCaching PublicStaticCaching
    else initCaching noCachePolicy
