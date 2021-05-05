{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Controller.Session where

import App.Model.Crypto
import App.Model.Database as DB
import App.Model.Env
import App.Model.User as User
import App.View.Page
import App.View.Session
import Control.Exception
import Control.Monad
import Data.List as L
import Data.Maybe
import Data.Text.Encoding
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Responder
import Web.Cookie

authErr = Just (InputError "email" "Incorrect email or password.")

getSigninForm :: Maybe AppError -> Responder AppEnv IO a
getSigninForm errM = do
  redirectIfRegisteredUser
  e <- fromMaybe "" <$> getParam "email"
  env <- getEnv
  sendHtmlPage (errorStatus errM)
    $ withLocation (TitledLocation "Sign In")
    $ withHtml
    $ signinFormHtml env errM e

post :: Responder AppEnv IO a
post = do
  redirectIfRegisteredUser
  env <- getEnv
  email <- maybe (getSigninForm authErr) pure =<< (fmap Email) <$> getParam "email"
  u <- maybe (getSigninForm authErr) pure =<< DB.exec (User.findOneByEmail email)
  p <- maybe (getSigninForm authErr) pure =<< getParam "password"
  when (not (User.verifyPassword u p)) (getSigninForm authErr)
  let res = redirect303 (renderUrl (appUrl env))
  sessCookie <- createSessionCookie u
  send (setCookie sessCookie res)

getUser :: Responder AppEnv IO (Maybe User)
getUser = appUser <$> getEnv

withUser :: AppEnv -> Request -> IO AppEnv
withUser env req = do
  let cookies = snd <$> L.find ((==) hCookie . fst) (requestHeaders req)
      cookieM = decodeUtf8 . snd <$> (L.find ((==) "sid" . fst) . parseCookies =<< cookies)
      userIdM = UUID.fromText =<< decrypt (appSecret env) =<< cookieM
  userM <- maybe (pure Nothing) (DB.execEnv env . User.findOne) userIdM
  return $ env {appUser = userM}

requireUser :: Responder AppEnv IO User
requireUser = do
  env <- getEnv
  let url = appUrl env +> ["sessions", "new"]
  maybe (send (redirect302 (renderUrl url))) pure =<< getUser

redirectIfRegisteredUser :: Responder AppEnv IO ()
redirectIfRegisteredUser = do
  userM <- getUser
  let isRegistered = maybe False userRegistered userM
  when isRegistered (send (redirect302 "/"))

getOrCreateUser :: Responder AppEnv IO User
getOrCreateUser = do
  env <- getEnv
  maybe (liftIO (createUser env Nothing)) pure =<< getUser
  where
    createUser env errM = do
      uid <- UUIDv4.nextRandom
      now <- getCurrentTime
      let user =
            User
              { userId = uid,
                userEmail = Nothing,
                userPassword = Nothing,
                userStatus = "unpaid",
                userPaidUntil = Nothing,
                userCanceledAt = Nothing,
                userStripeCustomerId = Nothing,
                userCreatedAt = now
              }
      handle (\(e :: SeldaError) -> maybe (createUser env (Just e)) throwIO errM) $ do
        DB.execEnv env (User.save user)
        return user

createSessionCookie :: User -> Responder AppEnv IO SetCookie
createSessionCookie user = do
  env <- getEnv
  now <- liftIO getCurrentTime
  let cookieValue =
        maybe
          "encrypt-failure"
          encodeUtf8
          (encrypt (appSecret env) (UUID.toText (userId user)))
      cookie =
        defaultSetCookie
          { setCookieName = "sid",
            setCookieValue = cookieValue,
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieExpires = Just $ addUTCTime 31536000 now
          }
  return cookie

delete :: Responder AppEnv IO ()
delete = do
  env <- getEnv
  maybe (send (redirect302 "/")) pure =<< getCookie "sid"
  now <- liftIO getCurrentTime
  let cookie =
        defaultSetCookie
          { setCookieName = "sid",
            setCookieValue = "",
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieExpires = Just $ addUTCTime (-10000) now
          }
  send $ setCookie cookie (redirect302 "/")
