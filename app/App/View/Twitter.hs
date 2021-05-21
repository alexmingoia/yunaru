{-# LANGUAGE OverloadedStrings #-}

module App.View.Twitter where

import App.Model.URL
import qualified Data.ByteString.Lazy as BL
import Data.List as L
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Text.Blaze.Html ((!), preEscapedToHtml, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Twitter.Types as TWT

tweetBlockquoteHtml :: Status -> Text
tweetBlockquoteHtml s = decodeUtf8 $ BL.toStrict $ renderHtml $ do
  H.blockquote $ do
    tweetBlockquoteAuthorHtml s
    tweetBlockquoteContentHtml s

tweetBlockquoteContentHtml s =
  preEscapedToHtml $ withBreakTags $ withoutRetweetPrefix (withUrls s)

tweetBlockquoteAuthorHtml s = do
  let u = statusUser s
      id = statusId s
      name = userName u
      url =
        fromJust $ parseUrl $
          "https://twitter.com/"
            <> userScreenName u
            <> "/status/"
            <> T.pack (show id)
  H.p $ do
    H.a ! A.href (urlValue url) $ toHtml $ "@" <> name
    toHtml (":" :: Text)

withoutRetweetPrefix :: Text -> Text
withoutRetweetPrefix s = fromMaybe s (T.stripPrefix "RT " s)

withBreakTags :: Text -> Text
withBreakTags txt = T.intercalate "<br/>" (T.lines txt)

twitterUrl username = fromJust $ parseUrl ("https://twitter.com/" <> username)

-- | Replace status entities with anchor tags.
withUrls :: TWT.Status -> Text
withUrls s =
  let txt = statusText s
      mediaEntity (Entity _ i) = (Nothing, i)
      urlLink txt = (\url -> (renderDisplayUrl url, renderUrl url)) <$> (parseUrl txt)
      urlEntity (Entity b i) = (urlLink (TWT.ueExpanded b), i)
      mentionEntity (Entity b i) = (Just ("@" <> TWT.userEntityUserScreenName b, renderUrl (twitterUrl (TWT.userEntityUserScreenName b))), i)
      mediaEntities = mediaEntity <$> maybe [] TWT.enMedia (TWT.statusEntities s)
      urlEntities = urlEntity <$> maybe [] TWT.enURLs (TWT.statusEntities s)
      mentionEntities = mentionEntity <$> maybe [] TWT.enUserMentions (TWT.statusEntities s)
      entities = L.sortOn (\(_, idx) -> listToMaybe idx) (mediaEntities <> urlEntities <> mentionEntities)
      entityAnchor Nothing = ""
      entityAnchor (Just (label, url)) = "<a href=\"" <> url <> "\">" <> label <> "</a>"
      linkify t b i ((link, (s : e : [])) : es) = linkify t (b <> (T.drop i (T.take s t)) <> entityAnchor link) e es
      linkify t b i (_ : es) = linkify t b i es
      linkify t _ 0 ([]) = t
      linkify _ b _ ([]) = b
   in withoutRetweetPrefix $ linkify txt "" 0 entities
