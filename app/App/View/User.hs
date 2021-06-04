{-# LANGUAGE OverloadedStrings #-}

module App.View.User where

import App.Model.Env
import App.Model.User
import App.View.Error
import App.View.Language
import App.View.Payment
import Control.Monad.Extra
import Data.Maybe
import Data.Text
import Data.UUID as UUID
import Text.Blaze.Html ((!), textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

userNewFormHtml env userM emailM passwordM confirmPasswordM errM = do
  let formActionUrl =
        case userId <$> userM of
          Nothing ->
            appUrl env +> ["users"]
          Just id ->
            appUrl env +> ["users", UUID.toText id] ?> [("_method", "PUT")]
  stripeJsHtml env
  H.section $ do
    H.h1 "Welcome."
    H.p $ do
      toHtml ("Create an account and save your feed for a one-time fee of " :: Text)
      H.strong "$19.99"
      toHtml (". You get unlimited usage, and an email you can use to follow newsletters. There's no tracking, no ads, and your data is kept private." :: Text)
    H.hr
    whenJust errM errorAlertHtml
    H.form
      ! A.method "POST"
      ! A.action (urlValue formActionUrl)
      ! A.enctype "multipart/form-data"
      ! A.onsubmit "submitFormAndRedirectToCheckout(event);"
      $ do
        H.div ! A.class_ "form-control" $ do
          H.label ! A.for "email" $ "Email Address"
          H.input
            ! A.name "email"
            ! A.type_ "email"
            ! A.value (textValue (maybe "" renderEmail emailM))
            ! A.required mempty
            ! A.placeholder "you@domain.com"
            ! A.class_ (errorClass "email" "input" errM)
          H.p $ H.small "We'll never share your e-mail, or send you spam."
        H.div ! A.class_ "form-control" $ do
          H.label ! A.for "password" $ "Password"
          H.input
            ! A.name "password"
            ! A.type_ "password"
            ! A.required mempty
            ! A.placeholder "Password"
            ! A.value (textValue (fromMaybe "" passwordM))
            ! A.class_ (errorClass "password" "input" errM)
        H.div ! A.class_ "form-control" $ do
          H.label ! A.for "password-confirm" $ "Confirm password"
          H.input
            ! A.name "password-confirm"
            ! A.type_ "password"
            ! A.required mempty
            ! A.placeholder "Confirm password"
            ! A.value (textValue (fromMaybe "" confirmPasswordM))
            ! A.class_ (errorClass "password-confirm" "input" errM)
        H.div ! A.class_ "form-control" $ do
          H.button ! A.type_ "submit" $ "Create account"

userEditFormHtml env user pendingEmailM emailM errM = do
  stripeJsHtml env
  when (not (userPaid user)) $ paymentIncompleteAlertHtml env user
  whenJust (userNewsletterId user) $ \newsletterId -> do
    H.section $ do
      H.h2 "Newsletters"
      H.p $ toHtml $ newsletterId <> "@" <> renderDisplayUrl (appUrl env)
      H.p $ H.small $ "Subscribe to newsletters using this email and they will appear in your feed."
  H.section $ do
    H.h2 "Account"
    whenJust errM errorAlertHtml
    whenJust pendingEmailM $ \email -> do
      if isJust (userEmail user)
        then do
          H.p ! A.role "alert" $ do
            H.span "Complete your email address change by clicking the link in the confirmation email sent to "
            H.strong (toHtml (renderEmail email))
            H.span "."
        else do
          H.p ! A.role "alert" $ do
            H.span "Confirm your email address by clicking the link in the email sent to "
            H.strong (toHtml (renderEmail email))
            H.span ". If it's in your spam folder, help us out by unmarking as spam."
    let passwordM = userPassword user
        formActionUrl = appUrl env +> ["users", UUID.toText (userId user)] ?> [("_method", "PUT")]
    H.form ! A.method "POST" ! A.action (urlValue formActionUrl) ! A.enctype "multipart/form-data" $ do
      let label = if isJust passwordM then "Change Password" else "Password"
          placeholder = if isJust passwordM then "New Password" else "Password"
      H.div ! A.class_ "form-control" $ do
        H.label ! A.for "email" $ "Email Address"
        H.input
          ! A.name "email"
          ! A.type_ "email"
          ! A.value (textValue (maybe "" renderEmail emailM))
          ! A.required mempty
          ! A.placeholder "you@domain.com"
          ! A.class_ (errorClass "email" "input" errM)
        H.p $ H.small "We'll never share your e-mail, or send you spam."
      H.div ! A.class_ "form-control" $ do
        H.label ! A.for "password" $ toHtml (label :: Text)
        H.input
          ! A.name "password"
          ! A.type_ "password"
          ! (if isNothing passwordM then A.required mempty else mempty)
          ! A.placeholder (textValue placeholder)
          ! A.class_ (errorClass "password" "input" errM)
      H.div ! A.class_ "form-control" $ do
        H.label ! A.for "password-confirm" $ toHtml $ "Confirm " <> label
        H.input
          ! A.name "password-confirm"
          ! A.type_ "password"
          ! (if isNothing passwordM then A.required mempty else mempty)
          ! A.placeholder (textValue ("Confirm " <> placeholder))
          ! A.class_ (errorClass "password-confirm" "input" errM)
      H.div ! A.class_ "form-control" $ do
        H.button ! A.type_ "submit" $ "Save"
  H.section $ do
    H.p $ do
      toHtml ("Questions or problems? Contact " :: Text)
      H.a
        ! A.href (textValue ("mailto:" <> renderEmail (appEmail env)))
        $ toHtml (renderEmail (appEmail env))
      toHtml ("." :: Text)
    let uid = UUID.toText (userId user)
    let formActionUrl = (appUrl env +> ["sessions", uid]) ?> [("_method", "DELETE")]
    H.form ! A.method "POST" ! A.action (urlValue formActionUrl) $ do
      H.small $ H.button ! A.class_ "button outline" ! A.type_ "submit" $ "Sign out"

timeHtml t = H.time ! A.datetime (textValue (formatTime8601 t)) $ toHtml $ formatTimeHuman t
