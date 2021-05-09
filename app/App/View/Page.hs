{-# LANGUAGE OverloadedStrings #-}

module App.View.Page where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed
import App.View.Icon as Icon
import App.View.Language
import Control.Monad.Zip (mzip)
import Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.UUID as UUID
import Network.Wai
import Network.Wai.Responder
import Text.Blaze.Html ((!), Html, customAttribute, textValue, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Cookie

appDesc :: Text
appDesc = "A peaceful news feed. Follow RSS, Twitter, newsletters, and more. No ads, no algorithm, no distractions."

data Page
  = Page
      { pageClass :: Maybe Text,
        pageLocation :: PageLocation,
        pageHead :: Html,
        pageMain :: Html,
        pageCookie :: Maybe SetCookie
      }

data PageLocation
  = FeedLocation FeedDetailed
  | EntryLocation EntryDetailed
  | TitledLocation Text
  | UntitledLocation

withHtml :: Html -> Page
withHtml html =
  Page
    { pageClass = Nothing,
      pageLocation = UntitledLocation,
      pageHead = mempty,
      pageMain = html,
      pageCookie = Nothing
    }

withHead html p = p {pageHead = pageHead p <> html}

withLocation loc p = p {pageLocation = loc}

withClass cls p = p {pageClass = Just cls}

withCookie cookie p = p {pageCookie = Just cookie}

sendHtmlPage :: Status -> Page -> Responder AppEnv IO a
sendHtmlPage status page = do
  env <- getEnv
  req <- getRequest
  let res = html status (renderPage env (Just req) page)
  send $ maybe res (flip setCookie res) (pageCookie page)

renderPage :: AppEnv -> Maybe Request -> Page -> BL.ByteString
renderPage env reqM p = renderHtml $ do
  H.docTypeHtml $ do
    H.head $ do
      case pageLocation p of
        UntitledLocation -> do
          H.title (toHtml (appName env))
          H.meta ! A.name "description" ! A.value (textValue appDesc)
        TitledLocation title -> do
          H.title (toHtml (title <> " | " <> appName env))
          H.meta ! A.name "description" ! A.value (textValue appDesc)
        FeedLocation feedDtld -> do
          H.title (toHtml (feedPageTitle feedDtld <> " | " <> appName env))
          H.meta ! A.name "description" ! A.value (textValue (feedPageDesc env feedDtld))
        EntryLocation entryDtld -> do
          let desc = fromMaybe appDesc (entrySummary (entryInfo entryDtld))
          H.title (toHtml (entryPageTitle entryDtld <> " | " <> appName env))
          H.meta ! A.name "description" ! A.value (textValue desc)
      defaultPageHead env
      H.script ! A.async mempty ! A.src (urlValue (appUrl env +> ["assets", "progressive-enhancements.js"])) $ mempty
      pageHead p
    H.body ! (maybe mempty (A.class_ . textValue) (pageClass p)) $ do
      pageHeader env reqM
      H.main (pageMain p)

defaultPageHead env = do
  H.meta ! A.charset "UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.meta ! A.name "mobile-web-app-capable" ! A.content "yes"
  H.link ! A.rel "manifest" ! A.href (urlValue (appUrl env +> ["assets", "manifest.json"]))
  if appProduction env
    then
      H.link
        ! A.type_ "text/css"
        ! A.rel "stylesheet"
        ! A.href (urlValue (appUrl env +> ["assets", "screen.min.css"]))
    else
      H.link
        ! A.type_ "text/css"
        ! A.rel "stylesheet"
        ! A.href (urlValue (appUrl env +> ["assets", "screen.css3.css"]))
  H.link
    ! A.rel "icon"
    ! A.type_ "image/png"
    ! A.sizes "512x512"
    ! A.href (urlValue (appUrl env +> ["assets", "sumi-news-512x512.png"]))
  H.link
    ! A.rel "apple-touch-icon"
    ! A.type_ "image/png"
    ! A.sizes "512x512"
    ! A.href (urlValue (appUrl env +> ["assets", "sumi-news-512x512.png"]))

pageHeader env reqM =
  H.header $ do
    H.nav $ do
      H.a ! A.href (urlValue (appUrl env)) ! A.title (textValue (appName env)) ! A.class_ "logo" $ toHtml $ appName env
      H.ul $ do
        let followingsHref = appUrl env +> ["followings"]
            signInHref = appUrl env +> ["sessions", "new"]
        H.li
          $ H.a
            ! A.href (urlValue (appUrl env))
            ! isActive reqM (appUrl env)
            ! A.title "News Feed"
          $ Icon.newspaper ! H.customAttribute "aria-label" "News Feed"
        H.li
          $ H.a
            ! A.href (urlValue followingsHref)
            ! isActive reqM followingsHref
            ! A.title "Followings"
          $ Icon.users ! H.customAttribute "aria-label" "Followings"
        case mzip (userEmail =<< appUser env) (appUser env) of
          Nothing -> do
            H.li
              $ H.a
                ! A.href (urlValue signInHref)
                ! isActive reqM signInHref
                ! A.title "Sign in"
              $ Icon.signIn ! H.customAttribute "aria-label" "Sign in"
          Just (_, u) -> do
            let uid = UUID.toText (userId u)
            H.li $ do
              let settingsUrl = (appUrl env +> ["users", uid, "edit"])
              H.a
                ! A.href (urlValue settingsUrl)
                ! isActive reqM settingsUrl
                ! A.title "Settings"
                $ Icon.user ! H.customAttribute "aria-label" "Settings"

isActive :: Maybe Request -> URL -> H.Attribute
isActive Nothing _ = mempty
isActive (Just req) url =
  let rh = decodeUtf8 <$> requestHeaderHost req
      uh = renderAuthority url
      rp = decodeUtf8 (rawPathInfo req)
      up = renderPath url
   in if rp == up && rh == uh
        then A.class_ "is-active"
        else mempty

ogImage imageUrl = H.meta ! customAttribute "property" "og:image" ! A.content (urlValue imageUrl)

ogTitle title = H.meta ! customAttribute "property" "og:title" ! A.content (textValue title)

ogDesc desc = H.meta ! customAttribute "property" "og:description" ! A.content (textValue desc)
