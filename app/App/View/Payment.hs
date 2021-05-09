module App.View.Payment where

import App.Model.Env
import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import Data.Maybe
import Data.Text
import Text.Blaze.Html ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

paymentFormHtml = do
  H.p "Redirecting you to payment page..."
  H.script "waitForScripts(redirectToCheckout);"

signupNoticeHtml env userM =
  whenJust userM $ \u -> when (isNothing (userEmail u)) $ do
    let signupUrl = appUrl env +> ["users", "new"]
    H.aside $ do
      H.p $ do
        H.a ! A.href (urlValue signupUrl) $ "Create an account"
        toHtml (" to save your followings for $33 per year. You'll also get an email you can use to read newsletters in your feed." :: Text)

paymentIncompleteAlertHtml env = do
  let paymentUrl = appUrl env +> ["payments", "new"]
  H.div ! A.role "alert" $ do
    H.p "Complete your payment to finish creating your account."
    H.form ! A.method "GET" ! A.action (urlValue paymentUrl) ! A.onsubmit "submitFormAndRedirectToCheckout(event);" $ do
      H.button ! A.type_ "submit" ! A.class_ "button outline" $ "Pay now"

stripeJsHtml env = do
  H.script $ toHtml $ "var stripeKey = '" <> appStripePublicKey env <> "';"
  H.script ! A.src "https://js.stripe.com/v3/" ! A.async mempty $ mempty
  H.script ! A.src (urlValue (appUrl env +> ["assets", "checkout.js"])) $ mempty
