{-# LANGUAGE OverloadedStrings #-}

module App.View.Error where

import App.Model.Env
import Control.Exception
import Data.Text
import Text.Blaze.Html ((!), AttributeValue, Html, textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Render an HTML error alert.
errorAlertHtml :: AppError -> Html
errorAlertHtml err = do
  H.div ! A.role "alert" $ do
    H.p $ toHtml $ displayException err

-- | Add a "has-error" class attribute for input errors.
errorClass :: Text -> Text -> Maybe AppError -> AttributeValue
errorClass name cls (Just (InputError n _)) = if name == n then textValue (cls <> " has-error") else textValue cls
errorClass _ cls _ = textValue cls
