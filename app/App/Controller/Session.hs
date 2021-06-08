{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Controller.Session where

import App.Controller.Page
import App.Model.Crypto
import App.Model.Database as DB
import App.Model.Env
import App.Model.Mnemonic as Mnemonic
import App.Model.User as User
import App.View.Session
import Control.Monad
import Data.Aeson as JSON
import Data.Maybe
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Web.Cookie
import Web.Twain

authErr = Just (InputError "email" "Incorrect email or password.")

getSigninForm :: Maybe AppError -> RouteM AppEnv a
getSigninForm errM = do
  redirectIfRegisteredUser
  email <- fromMaybe "" <$> paramMaybe "email"
  sendHtmlPage (errorStatus errM) "Sign in" $
    signinFormHtml errM email

post :: RouteM AppEnv a
post = do
  redirectIfRegisteredUser
  email <- maybe (getSigninForm authErr) pure =<< (fmap Email) <$> paramMaybe "email"
  u <- maybe (getSigninForm authErr) pure =<< DB.exec (User.findOneByEmail email)
  p <- maybe (getSigninForm authErr) pure =<< paramMaybe "password"
  when (not (User.verifyPassword u p)) (getSigninForm authErr)
  sessCookie <- createSessionCookie u
  send $ withCookie' sessCookie $ redirect303 "/"

getUser :: RouteM AppEnv (Maybe User)
getUser = do
  secret <- appSecret <$> env
  sessionM <- paramMaybe "session"
  case sessionM of
    Nothing -> do
      sidM <- paramMaybe "sid"
      let userIdM = UUID.fromText =<< decryptHex secret =<< sidM
      maybe (pure Nothing) (DB.exec . User.findOne) userIdM
    Just encryptedJson -> do
      return (decodeStrict =<< decrypt secret encryptedJson)

requireUser :: RouteM AppEnv User
requireUser = do
  maybe (send (redirect302 "/sessions/new")) pure =<< getUser

redirectIfRegisteredUser :: RouteM AppEnv ()
redirectIfRegisteredUser = do
  userM <- getUser
  let isRegistered = maybe False userRegistered userM
  when isRegistered (send (redirect302 "/"))

getOrCreateUser :: RouteM AppEnv User
getOrCreateUser = do
  userM <- getUser
  case userM of
    Nothing -> do
      id <- liftIO UUIDv4.nextRandom
      now <- liftIO getCurrentTime
      newsletterId <- liftIO Mnemonic.nextRandom
      let user = (emptyUser id now) {userNewsletterId = Just newsletterId}
      DB.exec $ User.save user
      return user
    Just user -> return user

createSessionCookie :: User -> RouteM AppEnv SetCookie
createSessionCookie user = do
  secret <- appSecret <$> env
  now <- liftIO getCurrentTime
  let userWithoutPass = user {userPassword = Nothing}
      userJson = encode userWithoutPass
      cookieValue = fromMaybe "encrypt-failure" $ encrypt secret userJson
      cookie =
        defaultSetCookie
          { setCookieName = "session",
            setCookieValue = cookieValue,
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieExpires = Just $ addUTCTime 31536000 now
          }
  return cookie

delete :: RouteM AppEnv ()
delete = do
  sidM <- paramMaybe "sid" :: RouteM AppEnv (Maybe Text)
  now <- liftIO getCurrentTime
  let expireCookie =
        defaultSetCookie
          { setCookieName = if isJust sidM then "sid" else "session",
            setCookieValue = "",
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieExpires = Just $ addUTCTime (-10000) now
          }
  send $ withCookie' expireCookie (redirect302 "/")
