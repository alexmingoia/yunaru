{-# LANGUAGE OverloadedStrings #-}

module App.Controller.MagicLink where

import App.Controller.Error as Error
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.Env
import App.Model.Following as Following
import App.Model.MagicLink as MagicLink
import App.Model.User as User
import App.View.MagicLink
import App.View.Page
import Control.Monad.Extra
import Data.Maybe
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Network.HTTP.Types
import Network.Wai.Responder

getForm err = do
  Session.redirectIfRegisteredUser
  sendHtmlPage (errorStatus err)
    $ withLocation (TitledLocation "Sign in")
    $ withHtml
    $ magicLinkFormHtml err

getSentMessage = do
  Session.redirectIfRegisteredUser
  sendHtmlPage status200
    $ withLocation (TitledLocation "Sign in")
    $ withHtml magicLinkSentHtml

post = do
  Session.redirectIfRegisteredUser
  emailP <- getParam "email"
  email <- maybe (getForm (Just (InputError "email" "Email cannot be blank."))) pure (parseEmail =<< emailP)
  mid <- liftIO UUIDv4.nextRandom
  expires <- addUTCTime 600 <$> liftIO getCurrentTime
  DB.exec $ MagicLink.save $ MagicLink mid email expires SignInMagicLink Nothing Nothing
  send (redirect303 "/magic-links/sent")

get :: Text -> Responder AppEnv IO ()
get midP = do
  let expiredError = InputError "url" "The magic link you used is expired."
      invalidError = InputError "url" "The magic link you used is invalid."
  mid <- maybe (Error.get invalidError) pure (UUID.fromText midP)
  DB.exec MagicLink.deleteExpired
  -- Verify magic link exists.
  magicLink <- maybe (Error.get expiredError) pure =<< DB.exec (MagicLink.findOne mid)
  -- Select existing session user, and user matching email.
  userFromEmailM <- DB.exec (User.findOneByEmail (magicLinkEmail magicLink))
  userFromSessionM <- Session.getUser
  -- Delete session user if different from user matching email.
  whenJust userFromEmailM $ \userFromEmail -> whenJust userFromSessionM $ \userFromSession ->
    when (isNothing (userEmail userFromSession) && userId userFromSession /= userId userFromEmail) $ do
      -- Merge followings and delete extraneous user.
      DB.exec (Following.updateUser userFromSession userFromEmail)
      DB.exec (User.delete userFromSession)
  -- Insert user or update existing user with email.
  user <- maybe Session.getOrCreateUser pure userFromEmailM
  when (isNothing (userEmail user) || magicLinkUserId magicLink == Just (userId user)) $ do
    let updatedUser = user {userEmail = Just (magicLinkEmail magicLink)}
    DB.exec (User.save updatedUser)
  sessionCookie <- Session.createSessionCookie user
  DB.exec (MagicLink.delete magicLink)
  send $ setCookie sessionCookie $ redirect302 "/"
