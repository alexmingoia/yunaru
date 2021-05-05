{-# LANGUAGE OverloadedStrings #-}

module App.Controller.User where

import App.Controller.NotFound as NotFound
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.Env
import App.Model.MagicLink as MagicLink
import App.Model.User as User
import App.View.Page
import App.View.Payment
import App.View.User
import Control.Applicative ((<|>))
import Control.Monad.Extra
import Data.Maybe
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Network.Wai
import Network.Wai.Responder

getForm :: Maybe AppError -> Responder AppEnv IO a
getForm errM = do
  userM <- Session.getUser
  case userM of
    Nothing -> getNewForm errM
    Just u ->
      if isJust (userEmail u)
        then getEditForm (UUID.toText (userId u)) errM
        else getNewForm errM

getNewForm :: Maybe AppError -> Responder AppEnv IO a
getNewForm errM = do
  userM <- Session.getUser
  env <- getEnv
  whenJust userM $ \u -> when (isJust (userEmail u)) $ do
    let userUrl = appUrl env +> ["users", UUID.toText (userId u), "edit"]
    send (redirect302 (renderUrl userUrl))
  emailM <- (parseEmail =<<) <$> getParam "email"
  passwordM <- getParam "password"
  confirmPasswordM <- getParam "confirm_password"
  sendHtmlPage (errorStatus errM)
    $ withLocation (TitledLocation "Create Account")
    $ withHead (stripeJsHtml env)
    $ withHtml
    $ userNewFormHtml env userM emailM passwordM confirmPasswordM errM

getEditForm :: Text -> Maybe AppError -> Responder AppEnv IO a
getEditForm id errM = do
  user <- Session.requireUser
  when (maybe True (/= userId user) (UUID.fromText id)) NotFound.get
  pendingEmailM <- DB.exec $ MagicLink.findOneVerifyEmail (userId user)
  emailM <- (<|> (userEmail user <|> pendingEmailM)) <$> ((parseEmail =<<) <$> getParam "email")
  env <- getEnv
  sendHtmlPage (errorStatus errM)
    $ withLocation (TitledLocation "Settings")
    $ withHead (stripeJsHtml env)
    $ withHtml
    $ userEditFormHtml env user pendingEmailM emailM errM

post :: Responder AppEnv IO ()
post = do
  user <- Session.getOrCreateUser
  res <- patchUser user
  sessionCookie <- Session.createSessionCookie user
  send (setCookie sessionCookie res)

put :: Text -> Responder AppEnv IO ()
put id = do
  user <- maybe NotFound.get pure =<< Session.getUser
  when (maybe True (/= userId user) (UUID.fromText id)) NotFound.get
  send =<< patchUser user

patchUser :: User -> Responder AppEnv IO Response
patchUser user = do
  env <- getEnv
  let userUrl = appUrl env +> ["users", UUID.toText (userId user), "edit"]
      passMatchError = InputError "password" "Passwords do not match."
      missingEmailError = InputError "email" "Email is required."
      respondError = getForm . Just
  passM <- getParam "password"
  passConfirmM <- getParam "password-confirm"
  emailM <- (parseEmail =<<) <$> getParam "email"
  email <- maybe (respondError missingEmailError) pure emailM
  when (passM /= passConfirmM) (respondError passMatchError)
  let userEmailM = lowercaseEmail <$> (userEmail user)
      paramEmailM = Just (lowercaseEmail email)
  when (userEmailM /= paramEmailM) $ do
    let existingUserErr =
          InputError
            "email"
            ( "There is another user with the email " <> renderEmail email
                <> ". If it's yours, sign in with that email."
            )
    existingUserM <- DB.exec (User.findOneByEmail email)
    when (isJust existingUserM) (respondError existingUserErr)
  case userEmailM of
    Nothing -> do
      case passM of
        Nothing -> DB.exec $ User.save (user {userEmail = Just email})
        Just pass -> do
          u <- liftIO (User.setPassword user pass)
          DB.exec $ User.save (u {userEmail = Just email})
    Just _ -> do
      mid <- liftIO UUIDv4.nextRandom
      expires <- addUTCTime 86400 <$> liftIO getCurrentTime
      let m = MagicLink mid email expires VerifyEmailMagicLink (Just (userId user)) Nothing
      DB.exec (MagicLink.save m)
      whenJust passM $ \pass -> do
        DB.exec . User.save =<< liftIO (User.setPassword user pass)
  returnNoContent <- maybe False (== "return-no-content") <$> getHeader "Prefer"
  if returnNoContent
    then return (plaintext status204 "")
    else
      if userPaid user
        then return $ redirect303 (renderUrl userUrl)
        else return $ redirect303 (renderUrl (appUrl env +> ["payments", "new"]))
