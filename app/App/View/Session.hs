{-# LANGUAGE OverloadedStrings #-}

module App.View.Session where

import App.Model.Env
import App.View.Error
import Control.Monad.Extra
import Data.Text
import Text.Blaze.Html ((!), Html, customAttribute, textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

signinFormHtml :: AppEnv -> Maybe AppError -> Text -> Html
signinFormHtml env eM u = do
  H.header ! A.class_ "hidden" $ H.h1 "Sign in"
  H.form ! A.method "POST" ! A.action "/sessions" ! A.class_ "centered" $ do
    whenJust eM errorAlertHtml
    H.div ! A.class_ "form-control" $ do
      H.label ! A.for "email" ! A.class_ "hidden" $ "Email"
      H.input
        ! A.name "email"
        ! A.type_ "text"
        ! customAttribute "autocapitalize" "none"
        ! A.required mempty
        ! A.placeholder "you@domain.com"
        ! A.autofocus mempty
        ! A.value (textValue u)
        ! A.class_ "input"
    H.div ! A.class_ "form-control" $ do
      H.label ! A.for "password" ! A.class_ "hidden" $ "Password"
      H.input
        ! A.name "password"
        ! A.type_ "password"
        ! A.required mempty
        ! A.placeholder "Password"
        ! A.class_ "input"
    H.div ! A.class_ "form-control" $ do
      H.button ! A.type_ "submit" $ "Sign in"
    H.p $ H.small $ do
      let magicLinkHref = appUrl env +> ["magic-links", "new"]
      toHtml ("Forgot your password? " :: Text)
      H.a ! A.href (urlValue magicLinkHref) $ "Sign in with magic link"
      toHtml ("." :: Text)
    H.p $ H.small $ do
      let signupUrl = appUrl env +> ["users", "new"]
      H.a ! A.href (urlValue signupUrl) ! A.class_ "button outline" $ "Create an account"
