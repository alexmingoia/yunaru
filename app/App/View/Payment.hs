module App.View.Payment where

import App.Model.Env
import App.Model.User
import Data.Text
import Text.Blaze.Html ((!), textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

paymentFormHtml env = do
  H.p "Redirecting you to payment page..."
  stripeJsHtml env
  H.script "waitForScripts(redirectToCheckout);"

paymentAlertHtml env user = do
  let paymentUrl = appUrl env +> ["payments", "new"]
  H.div ! A.role "alert" $ do
    H.p $ do
      toHtml ("Free accounts are limited to 10 followings. Unlock unlimited usage for a one-time fee of " :: Text)
      H.strong "$19.99"
      toHtml ("." :: Text)
    if userRegistered user
      then do
        H.form
          ! A.method "GET"
          ! A.action (urlValue paymentUrl)
          ! A.onsubmit "submitFormAndRedirectToCheckout(event);"
          $ do
            let email = maybe "" renderEmail (userEmail user)
            H.input ! A.type_ "hidden" ! A.name "email" ! A.value (textValue email)
            H.button ! A.type_ "submit" ! A.class_ "button outline" $ "Unlock"
        stripeJsHtml env
      else H.a ! A.href "/users/new?pay" ! A.class_ "button outline" $ "Unlock"

stripeJsHtml env = do
  H.script $ toHtml $ "var stripeKey = '" <> appStripePublicKey env <> "';"
  H.script ! A.src "https://js.stripe.com/v3/" ! A.async mempty $ mempty
  H.script ! A.src (urlValue (appUrl env +> ["assets", "checkout.js"])) $ mempty
