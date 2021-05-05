{-# LANGUAGE OverloadedStrings #-}

module App.View.MagicLink where

import App.Model.Env
import App.Model.MagicLink
import App.View.Error
import Control.Lens
import Control.Monad.Extra
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Text
import Data.Text.Encoding
import Data.UUID as UUID
import Network.AWS.SES as SES
import Text.Blaze.Html ((!), toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

magicLinkFormHtml err = do
  H.header ! A.class_ "hidden" $ H.h1 "Sign in"
  H.form ! A.method "POST" ! A.action "/magic-links" ! A.class_ "centered" $ do
    whenJust err errorAlertHtml
    H.div ! A.class_ "form-controls-inline" $ do
      H.div ! A.class_ "form-control" $ do
        H.label ! A.for "email" ! A.class_ "hidden" $ "Email"
        H.input
          ! A.name "email"
          ! A.type_ "email"
          ! A.required mempty
          ! A.placeholder "you@domain.com"
          ! A.autofocus mempty
          ! A.class_ "input"
      H.div ! A.class_ "form-control" $ do
        H.button ! A.type_ "submit" $ "Sign in"

magicLinkSentHtml = do
  H.header $ H.h1 $ "Check your email."
  H.section $ do
    H.p $ do
      toHtml ("We sent you a magic link to sign in." :: Text)
      H.br
      toHtml ("If it's in your spam folder, help us by unmarking as spam." :: Text)

magicLinkEmailMessage :: AppEnv -> MagicLink -> Message
magicLinkEmailMessage env magicLink =
  case magicLinkAction magicLink of
    SignInMagicLink -> emailSignInMessage env magicLink
    VerifyEmailMagicLink -> emailVerifyEmailMessage env magicLink

emailSignInMessage env magicLink =
  let magicLinkHref = appUrl env +> ["magic-links", UUID.toText (magicLinkId magicLink)]
      msgSubject = content $ "Sign in to " <> appName env
      msgBody = set bHTML (Just html) $ set bText (Just plaintext) body
      html = content $ decodeUtf8 . B.concat . BL.toChunks $ renderHtml $ do
        H.p "Hello!"
        H.p $ toHtml $
          "Use the link below to sign in to your " <> appName env <> " account. This link expires in 15 minutes."
        H.p $ H.strong
          $ H.a ! A.href (urlValue magicLinkHref)
          $ toHtml
          $ "Sign in to " <> appName env
        H.p "If you didn't make this request, you can safely ignore this email."
        H.p $ do
          H.span "Happy blogging,"
          H.br
          H.span $ toHtml $ appName env <> " robot"
        H.hr
        H.p "If you're having trouble with the link above, copy and paste the URL below into your web browser:"
        H.p $ toHtml (renderUrl magicLinkHref)
      plaintext =
        content $
          "Hello!\n\
          \Use the link below to log in to your "
            <> appName env
            <> " account. This link expires in 15 minutes.\n\
               \"
            <> renderUrl magicLinkHref
            <> "\n\
               \If you didn't make this request, you can safely ignore this email.\n\
               \Happy blogging,\n\
               \"
            <> appName env
            <> " robot"
   in message msgSubject msgBody

emailVerifyEmailMessage env magicLink =
  let magicLinkHref = appUrl env +> ["magic-links", UUID.toText (magicLinkId magicLink)]
      msgSubject = content $ "Confirm email address for " <> appName env
      msgBody = set bHTML (Just html) $ set bText (Just plaintext) body
      html = content $ decodeUtf8 . B.concat . BL.toChunks $ renderHtml $ do
        H.p "Hello!"
        H.p $ toHtml $
          "Use the link below to confirm the email address for your " <> appName env
            <> " account. This link expires in 15 minutes."
        H.p $ H.strong
          $ H.a ! A.href (urlValue magicLinkHref)
          $ toHtml
          $ "Confirm email address for " <> appName env
        H.p "If you didn't make this request, you can safely ignore this email."
        H.p $ do
          H.span "Happy blogging,"
          H.br
          H.span $ toHtml $ appName env <> " robot"
        H.hr
        H.p "If you're having trouble with the link above, copy and paste the URL below into your web browser:"
        H.p $ toHtml (renderUrl magicLinkHref)
      plaintext =
        content $
          "Hello!\n\
          \Use the link below to confirm the email address for your "
            <> appName env
            <> " account. This link expires in 15 minutes.\n\
               \"
            <> renderUrl magicLinkHref
            <> "\n\
               \If you didn't make this request, you can safely ignore this email.\n\
               \Happy blogging,\n\
               \"
            <> appName env
            <> " robot"
   in message msgSubject msgBody
