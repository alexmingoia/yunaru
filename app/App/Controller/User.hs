{-# LANGUAGE OverloadedStrings #-}

module App.Controller.User where

import App.Controller.NotFound as NotFound
import App.Controller.Page
import App.Controller.Session as Session
import App.Model.Database as DB hiding (text)
import App.Model.Env
import App.Model.MagicLink as MagicLink
import App.Model.User as User
import App.View.User
import Control.Applicative ((<|>))
import Control.Monad.Extra
import Data.Maybe
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Network.Wai
import Web.Twain

getForm :: Maybe AppError -> RouteM AppEnv a
getForm errM = do
  userM <- Session.getUser
  case userM of
    Nothing -> getNewForm errM
    Just u ->
      if isJust (userEmail u)
        then getEditForm (UUID.toText (userId u)) errM
        else getNewForm errM

getNewForm :: Maybe AppError -> RouteM AppEnv a
getNewForm errM = do
  userM <- Session.getUser
  whenJust userM $ \u -> when (isJust (userEmail u)) $ do
    let userUrl = rootUrl +> ["users", UUID.toText (userId u), "edit"]
    send (redirect302 (renderUrl userUrl))
  emailM <- paramMaybe "email"
  passwordM <- paramMaybe "password"
  confirmPasswordM <- paramMaybe "confirm_password"
  appEnv <- env
  sendHtmlPage (errorStatus errM) "Create Account" $
    userNewFormHtml appEnv userM emailM passwordM confirmPasswordM errM

getEditForm :: Text -> Maybe AppError -> RouteM AppEnv a
getEditForm id errM = do
  user <- Session.requireUser
  when (maybe True (/= userId user) (UUID.fromText id)) NotFound.get
  pendingEmailM <- DB.exec $ MagicLink.findOneVerifyEmail (userId user)
  emailM <- (<|> (userEmail user <|> pendingEmailM)) <$> paramMaybe "email"
  appEnv <- env
  sendHtmlPage (errorStatus errM) "Settings" $
    userEditFormHtml appEnv user pendingEmailM emailM errM

post :: RouteM AppEnv ()
post = do
  user <- Session.getOrCreateUser
  sessionCookie <- Session.createSessionCookie user
  send . withCookie' sessionCookie =<< patchUser user

put :: RouteM AppEnv ()
put = do
  id <- param "id"
  user <- maybe NotFound.get pure =<< Session.getUser
  when (maybe True (/= userId user) (UUID.fromText id)) NotFound.get
  send =<< patchUser user

patchUser :: User -> RouteM AppEnv Response
patchUser user = do
  e <- env
  let userUrl = appUrl e +> ["users", UUID.toText (userId user), "edit"]
      passMatchError = InputError "password" "Passwords do not match."
      missingEmailError = InputError "email" "Email is required."
      respondError = getForm . Just
  passM <- paramMaybe "password"
  passConfirmM <- paramMaybe "password-confirm"
  emailM <- (parseEmail =<<) <$> paramMaybe "email"
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
  returnNoContent <- maybe False (== "return-no-content") <$> header "Prefer"
  if returnNoContent
    then return $ status status204 $ text ""
    else
      if userPaid user
        then return $ redirect303 (renderUrl userUrl)
        else return $ redirect303 (renderUrl (appUrl e +> ["payments", "new"]))
