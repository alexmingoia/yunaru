{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App.View.Feed where

import App.Model.Author
import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed
import App.Model.Following
import App.Model.Image as Image
import App.View.Entry
import App.View.Icon as Icon
import App.View.Language
import App.View.Page
import App.View.URL
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Data.List as L
import Data.Maybe
import Data.Text as T
import Text.Blaze.Html ((!), customAttribute, preEscapedToHtml, textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax (RSS (..), RSSChannel (..), RSSImage (..))

feedHtml env now feedDtld entriesDtld beforeM pageSize = do
  feedHeaderHtml env feedDtld
  forM_ entriesDtld (entrySnippetHtml env now)
  when (isJust beforeM && pageSize /= L.length entriesDtld) (endOfFeedNoticeHtml feedDtld)
  when (isNothing beforeM && L.null entriesDtld) (noEntriesNoticeHtml env feedDtld)
  when (pageSize == L.length entriesDtld) $ do
    let leastPublishedAtM = entryRebloggedOrPublishedAt (entryInfo (L.minimum entriesDtld))
    whenJust leastPublishedAtM $ \ts -> do
      let nextPageUrl = localFeedUrl env (feedInfo feedDtld) ?> [("before", formatTime8601 ts)]
      H.nav $ H.small $ do
        H.a ! A.href (urlValue nextPageUrl) ! A.class_ "button" $ "More entries →"

feedHeaderHtml env feedDtld = do
  let feed = feedInfo feedDtld
      followingM = feedFollowing feedDtld
      author = feedAuthor feedDtld
  H.header ! A.class_ "feed-profile" $ do
    let authorImageSrcM = Image.cacheUrl env 128 128 True <$> authorImageUrl author
    whenJust authorImageSrcM $ \src -> do
      H.div ! A.class_ "image" $ H.img ! A.src (urlValue src) ! A.width "64" ! A.height "64" ! A.class_ "u-photo"
    H.h1
      ! (if isJust (feedName feed) then A.class_ "p-name" else mempty)
      $ toHtml
      $ fromMaybe (feedDisplayName feedDtld) (authorName author)
    H.p $ H.small $ do
      if authorUrl author == feedUrl feed
        then do
          let hrefUrl = canonicalFeedUrl env feed
              displayUrl = renderDisplayUrl hrefUrl
          feedIcon (feedFormat feed)
          toHtml (" " :: Text)
          H.a ! A.href (urlValue hrefUrl) ! A.class_ "u-url" $ toHtml displayUrl
        else do
          Icon.globe
          toHtml (" " :: Text)
          H.a ! A.href (urlValue (authorUrl author)) $ toHtml $ renderDisplayUrl (authorUrl author)
          H.br
          feedIcon (feedFormat feed)
          toHtml (" " :: Text)
          case feedFormat feed of
            EmailFeedFormat -> do
              H.span ! A.class_ "p-name" $ toHtml (feedDisplayName feedDtld)
            _ -> do
              when (feedName feed /= authorName author) $ do
                H.span ! A.class_ "p-name" $ toHtml (feedDisplayName feedDtld)
                toHtml (": " :: Text)
              H.a ! A.href (urlValue (feedUrl feed)) ! A.class_ "u-url" $ do
                toHtml (renderDisplayUrl (feedUrl feed))
    whenJust (feedSummary feed <|> authorNote author) $ \summary -> do
      H.p ! A.class_ "p-summary" $ preEscapedToHtml summary
    H.div ! A.class_ "form-controls-inline" $ do
      feedFollowButtonHtml env followingM feedDtld
      toHtml (" " :: Text)
      feedMuteButtonHtml env followingM feedDtld
    H.div ! A.class_ "p-author h-card hidden" $ do
      let url = canonicalAuthorUrl env author
      case authorName author of
        Nothing -> H.a ! A.href (urlValue url) ! A.class_ "u-url" $ toHtml (renderDisplayUrl url)
        Just name -> H.a ! A.href (urlValue url) ! A.class_ "u-url p-name" $ toHtml name

feedIcon EmailFeedFormat = Icon.newsletter
feedIcon TwitterFeedFormat = Icon.twitter
feedIcon MF2FeedFormat = Icon.globe
feedIcon _ = Icon.rss

feedFollowButtonHtml env followingM feedDtld = do
  let isFollowing = isJust followingM
      feed = feedInfo feedDtld
      followFormMethod = if isFollowing then "DELETE" else "PUT"
      followFormMethodXHR = if isFollowing then "PUT" else "DELETE"
      followFormAction = appUrl env +> ["followings", renderUrl (feedUrl feed)] ?> [("_method", followFormMethod)]
      followFormActionXHR = appUrl env +> ["followings", renderUrl (feedUrl feed)] ?> [("_method", followFormMethodXHR)]
  when isFollowing $ do
    H.form
      ! A.method "POST"
      ! A.action (urlValue followFormAction)
      ! A.class_ "small"
      ! customAttribute "data-xhr" "true"
      ! customAttribute "data-xhr-success-label" (if isFollowing then "Follow" else "Unfollow")
      ! customAttribute "data-xhr-success-action" (urlValue followFormActionXHR)
      $ do
        H.input ! A.type_ "hidden" ! A.name "redirect_url" ! A.value (urlValue (localFeedUrl env feed))
        H.button ! A.type_ "submit" ! A.class_ "button outline" $ if isFollowing then "Unfollow" else "Follow"

feedMuteButtonHtml env followingM feedDtld = do
  let feed = feedInfo feedDtld
      isFollowing = isJust followingM
      isMuted = maybe False followingMuted followingM
      muteFormAction = appUrl env +> ["followings", renderUrl (feedUrl feed)] ?> [("_method", "PUT")]
  when isFollowing $ do
    H.form
      ! A.method "POST"
      ! A.action (urlValue muteFormAction)
      ! A.class_ "small"
      ! customAttribute "data-xhr" "true"
      ! customAttribute "data-xhr-success-label" (if isMuted then "Mute" else "Unmute")
      ! customAttribute "data-xhr-success-action" (urlValue muteFormAction)
      $ do
        H.input ! A.type_ "hidden" ! A.name "redirect_url" ! A.value (urlValue (localFeedUrl env feed))
        H.input ! A.type_ "hidden" ! A.name "muted" ! A.value (textValue (if isMuted then "False" else "True"))
        H.button ! A.type_ "submit" ! A.class_ "button outline" $ if isMuted then "Unmute" else "Mute"

noEntriesNoticeHtml env feedDtld = do
  H.p $ toHtml $
    authorDisplayName env (feedAuthor feedDtld) <> " does not have any entries."

endOfFeedNoticeHtml feedDtld =
  H.p $ toHtml $ "You've reached the end of " <> possessive (feedDisplayName feedDtld) <> " blog."

feedXml env feedDtld entriesDtld =
  let feed = feedInfo feedDtld
      author = feedAuthor feedDtld
   in textRSS $
        RSS
          { rssVersion = "2.0",
            rssAttrs = [],
            rssOther = [],
            rssChannel =
              RSSChannel
                { rssTitle = feedDisplayName feedDtld,
                  rssLink = renderUrl (localFeedUrl env feed),
                  rssDescription = fromMaybe ("Latest entries for " <> feedDisplayName feedDtld) (feedSummary feed),
                  rssItems = entryXml env <$> entriesDtld,
                  rssLanguage = Nothing,
                  rssCopyright = Nothing,
                  rssEditor = Nothing,
                  rssWebMaster = Nothing,
                  rssPubDate = Nothing,
                  rssLastUpdate = formatTime822 <$> feedUpdatedAt feed,
                  rssCategories = [],
                  rssGenerator = Nothing,
                  rssDocs = Nothing,
                  rssCloud = Nothing,
                  rssTTL = Nothing,
                  rssImage = feedImageXml env feed author <$> authorImageUrl author,
                  rssRating = Nothing,
                  rssTextInput = Nothing,
                  rssSkipHours = Nothing,
                  rssSkipDays = Nothing,
                  rssChannelOther = []
                }
          }

feedImageXml env feed author imageUrl =
  RSSImage
    { rssImageURL = renderUrl (Image.cacheUrl env 128 128 True imageUrl),
      rssImageTitle = fromMaybe "" (feedName feed <|> authorName author),
      rssImageLink = renderUrl (localFeedUrl env feed),
      rssImageWidth = Just 128,
      rssImageHeight = Just 128,
      rssImageDesc = Nothing,
      rssImageOther = []
    }

feedRssLink env feed = do
  let href = if isNothing (feedImportedAt feed) then canonicalFeedUrl env feed +> ["rss"] else feedUrl feed
  H.link ! A.rel "alternate" ! A.type_ "application/rss+xml" ! A.href (urlValue href)

feedMetaTags env feedDtld = do
  let f = feedInfo feedDtld
      a = feedAuthor feedDtld
      title = fromMaybe (renderDisplayUrl (feedUrl f)) (feedName f <|> authorName a)
  feedRssLink env f
  H.meta ! customAttribute "property" "twitter:card" ! A.content "summary"
  ogTitle title
  ogDesc (feedPageDesc env feedDtld)
  whenJust (authorImageUrl a) (ogImage . Image.cacheUrl env 180 180 True)
