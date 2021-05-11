{-# LANGUAGE OverloadedStrings #-}

module App.View.Page where

import App.Model.Env
import App.Model.User
import App.View.Icon as Icon
import Control.Monad.Zip (mzip)
import qualified Data.ByteString.Lazy as BL
import Data.Text as T
import Data.UUID as UUID
import Text.Blaze.Html ((!), Html, customAttribute, textValue, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

appDesc :: Text
appDesc = "A peaceful news feed. Follow RSS, Twitter, newsletters, and more. No ads, no algorithm, no distractions."

renderPage :: Text -> Text -> Text -> Maybe User -> Html -> BL.ByteString
renderPage appName title reqPath userM html = renderHtml $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title
        $ toHtml
        $ if T.null title || title == appName
          then appName
          else title <> " | " <> appName
      H.meta ! A.name "description" ! A.value (textValue appDesc)
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "mobile-web-app-capable" ! A.content "yes"
      H.link ! A.rel "manifest" ! A.href "/assets/manifest.json"
      H.link
        ! A.type_ "text/css"
        ! A.rel "stylesheet"
        ! A.href "/assets/screen.min.css"
      H.link
        ! A.rel "icon"
        ! A.type_ "image/png"
        ! A.sizes "512x512"
        ! A.href "/assets/sumi-news-512x512.png"
      H.link
        ! A.rel "apple-touch-icon"
        ! A.type_ "image/png"
        ! A.sizes "512x512"
        ! A.href "/assets/sumi-news-512x512.png"
      H.script
        ! A.async mempty
        ! A.src "/assets/progressive-enhancements.js"
        $ mempty
    H.body $ do
      pageHeader appName reqPath userM
      H.main html

pageHeader appName reqPath userM =
  H.header $ do
    H.nav $ do
      H.a
        ! A.href "/"
        ! A.title (textValue appName)
        ! A.class_ "logo"
        $ toHtml appName
      H.ul $ do
        let followingsHref = "/followings"
            signInHref = "/sessions/new"
        H.li
          $ H.a
            ! A.href "/"
            ! isActive reqPath "/"
            ! A.title "News Feed"
          $ Icon.newspaper ! H.customAttribute "aria-label" "News Feed"
        H.li
          $ H.a
            ! A.href (textValue followingsHref)
            ! isActive reqPath followingsHref
            ! A.title "Followings"
          $ Icon.users ! H.customAttribute "aria-label" "Followings"
        case mzip (userEmail =<< userM) userM of
          Nothing -> do
            H.li
              $ H.a
                ! A.href (textValue signInHref)
                ! isActive reqPath signInHref
                ! A.title "Sign in"
              $ Icon.signIn ! H.customAttribute "aria-label" "Sign in"
          Just (_, u) -> do
            let uid = UUID.toText (userId u)
            H.li $ do
              let settingsUrl = "/users/" <> uid <> "/edit"
              H.a
                ! A.href (textValue settingsUrl)
                ! isActive reqPath settingsUrl
                ! A.title "Settings"
                $ Icon.user ! H.customAttribute "aria-label" "Settings"

isActive :: Text -> Text -> H.Attribute
isActive reqPath urlPath =
  if reqPath == urlPath then A.class_ "is-active" else mempty

ogImage imageUrl =
  H.meta
    ! customAttribute "property" "og:image"
    ! A.content (urlValue imageUrl)

ogTitle title =
  H.meta
    ! customAttribute "property" "og:title"
    ! A.content (textValue title)

ogDesc desc =
  H.meta
    ! customAttribute "property" "og:description"
    ! A.content (textValue desc)
