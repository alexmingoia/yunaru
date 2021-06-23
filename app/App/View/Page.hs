{-# LANGUAGE OverloadedStrings #-}

module App.View.Page where

import App.Model.Env
import App.Model.User
import App.View.Icon as Icon
import Control.Monad.Zip (mzip)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text as T
import Data.UUID as UUID
import Text.Blaze.Html ((!), Html, customAttribute, textValue, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

appDesc :: Text
appDesc = "A peaceful news feed. Follow RSS, Twitter, newsletters, and more. No ads, algorithms, or distractions."

renderPage :: Text -> Text -> Text -> Maybe User -> Html -> BL.ByteString
renderPage appName title reqPath userM html = renderHtml $ do
  let pageTitle =
        if T.null title || title == appName
          then appName
          else title <> " | " <> appName
  H.docTypeHtml $ do
    H.head $ do
      H.title $ toHtml pageTitle
      H.meta ! A.name "description" ! A.content (textValue appDesc)
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "mobile-web-app-capable" ! A.content "yes"
      H.link ! A.rel "manifest" ! A.href "/assets/manifest.json"
      H.link
        ! A.type_ "text/css"
        ! A.rel "stylesheet"
        ! A.href "/assets/screen.min.css"
      H.link
        ! A.rel "mask-icon"
        ! A.href "/assets/sumi.news.svg"
      H.link
        ! A.rel "icon"
        ! A.type_ "image/png"
        ! A.href "/assets/sumi.news-favicon-512x512.png"
      H.link
        ! A.rel "apple-touch-icon"
        ! A.type_ "image/png"
        ! A.sizes "512x512"
        ! A.href "/assets/sumi.news-512x512.png"
      H.meta ! A.name "twitter:card" ! A.content "summary"
      H.meta ! A.name "twitter:title" ! A.content (textValue pageTitle)
      H.meta ! A.name "twitter:description" ! A.content (textValue appDesc)
      H.meta ! A.name "twitter:image" ! A.content "/assets/sumi.news-512x512.png"
      H.meta ! A.name "twitter:creator" ! A.content "@tofukidxyz"
      H.meta ! A.name "og:image" ! A.content "/assets/sumi.news-512x512.png"
      H.script
        ! A.async mempty
        ! A.src "/assets/progressive-enhancements.js"
        $ mempty
    H.body $ renderBody appName reqPath userM html

renderBody :: Text -> Text -> Maybe User -> Html -> Html
renderBody appName reqPath userM html = do
  pageHeader appName reqPath userM
  H.main html

renderBody' :: Text -> Text -> Maybe User -> Html -> BL.ByteString
renderBody' appName reqPath userM html =
  renderHtml $ renderBody appName reqPath userM html

pageHeader appName reqPath userM = do
  H.header $ do
    H.nav $ do
      H.a
        ! A.href "/"
        ! A.title (textValue appName)
        ! A.class_
          ( if reqPath == "/" && isJust userM
              then "logo icon-left is-active"
              else "logo icon-left"
          )
        $ do
          Icon.logo
          toHtml appName
      H.ul $ do
        let followingsHref = "/followings"
            signInHref = "/sessions/new"
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
            H.li $ do
              H.a
                ! A.href "/users/new"
                ! A.class_ "button outline"
                $ (if isJust userM then "Save Feed" else "Create Account")
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
