{-# LANGUAGE OverloadedStrings #-}

module App.View.Following where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed
import App.Model.FollowingDetailed
import App.Model.Image as Image
import App.Model.User
import App.View.Error
import App.View.Icon as Icon
import App.View.Language
import App.View.Payment
import App.View.URL
import Control.Monad.Extra
import Data.List as L
import Data.Maybe
import Data.Text as T
import Text.Blaze.Html ((!), customAttribute, preEscapedToHtml, textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

followingsRecentEntryHtml env now pageSize beforeM userM err urlP followingsDtld = do
  let formActionUrl = rootUrl +> ["followings"]
      isPaid = maybe False userPaid userM
      followedLimit = L.length followingsDtld >= 10 && not isPaid
  whenJust userM $ \user -> do
    when followedLimit $ paymentAlertHtml env user
  H.form
    ! A.method "POST"
    ! A.action (urlValue formActionUrl)
    ! (if followedLimit then A.disabled "disabled" else mempty)
    $ do
      whenJust err errorAlertHtml
      H.div ! A.class_ "form-controls-inline" $ do
        H.div ! A.class_ "form-control" $ do
          H.label ! A.class_ "hidden" ! A.for "url" $ "URL to follow"
          H.input
            ! A.name "url"
            ! A.type_ "text"
            ! A.required mempty
            ! A.placeholder "Blog / RSS / Twitter / Tumblr / YouTube"
            ! A.value (textValue urlP)
            ! A.class_ "input"
        H.div ! A.class_ "form-control" $ do
          H.button ! A.type_ "submit" $ "Follow"
  forM_ followingsDtld (followingRecentEntrySnippetHtml env now)
  when (isNothing beforeM && L.null followingsDtld) noFollowingsNoticeHtml
  when (L.length followingsDtld == 1) firstFollowingNoticeHtml
  when (pageSize == L.length followingsDtld) $ do
    let leastUpdatedAtM = feedUpdatedAt (followingFeed (L.minimum followingsDtld))
    whenJust leastUpdatedAtM $ \leastUpdatedAt -> do
      let nextPageUrl = rootUrl +> ["followings"] ?> [("before", formatTime8601 leastUpdatedAt)]
      H.nav $ do
        H.a
          ! A.href (urlValue nextPageUrl)
          ! A.class_ "button"
          $ "More followings →"
  when (L.length followingsDtld > 1) $ do
    H.p $ H.small $ do
      H.a ! A.href "/discover" $ "Discover"
      toHtml (" new feeds." :: Text)

noFollowingsNoticeHtml = do
  H.p $ "Your followings will appear here, along with their most recent entry."

firstFollowingNoticeHtml = do
  H.p $ H.small $ do
    toHtml ("Followings are listed with their most recent post. Visit your " :: Text)
    H.a ! A.href "/" ! A.class_ "icon-left" $ do
      Icon.newspaper
      H.span "News Feed"
    toHtml (" to see all posts. Looking for more feeds to follow? " :: Text)
    H.a ! A.href "/discover" $ "Discover"
    toHtml (" new feeds." :: Text)

followingRecentEntrySnippetHtml env now followingDtld = do
  let entryM = followingRecentEntry followingDtld
      isMuted = followingMuted (followingInfo followingDtld)
      feed = followingFeed followingDtld
      author = followingAuthor followingDtld
      feedDtld = feedDetailed feed author
  H.div ! A.class_ "h-cite u-follow-of" $ do
    H.div ! A.class_ "byline" $ do
      whenJust (Image.cacheUrl env 128 128 True <$> authorImageUrl author) $ \imageUrl -> do
        H.a ! A.href (urlValue (localFeedUrl feed)) ! A.class_ "image" $ do
          H.img
            ! A.class_ "u-photo"
            ! A.src (urlValue imageUrl)
            ! A.width "32"
            ! A.height "32"
            ! customAttribute "aria-hidden" "true"
            ! A.alt (textValue (feedDisplayName feedDtld))
      H.a ! A.href (urlValue (canonicalAuthorUrl author)) ! A.class_ "p-name" $
        toHtml (feedDisplayName feedDtld)
      when isMuted $ do
        H.span " · "
        H.a ! A.href (urlValue (localFeedUrl feed)) $ H.span $ "Muted"
      whenJust (entryRebloggedOrPublishedAt =<< entryM) $ \ts -> do
        H.span " · "
        H.time
          ! A.datetime (textValue (formatTime8601 ts))
          ! A.title (textValue (formatTimeHuman ts))
          $ toHtml (formatTimeAgoCompact now ts)
      H.a
        ! A.href (urlValue (localFeedUrl feed))
        ! A.title "Feed"
        $ Icon.ellipsis
    case entryM of
      Nothing -> do
        H.p ! A.class_ "p-summary" $ "No recent entry."
      Just e -> do
        H.p ! A.class_ "p-summary" $ do
          when (isJust (entryRebloggedAt e)) $ do
            Icon.reblog ! customAttribute "aria-label" "reblog"
            toHtml (" " :: Text)
          whenJust (entryName e) $ \name -> do
            H.a ! A.href (urlValue (localEntryUrl e)) $ toHtml name
            when (isJust (entrySummary e)) (toHtml (": " :: Text))
          whenJust (entrySummary e) $ \summary -> do
            preEscapedToHtml summary
            toHtml (" " :: Text)
          when (not (L.null (entryImageUrls e))) $ do
            forM_ (entryImageUrls e) $ \imageUrl -> do
              H.a ! A.href (urlValue imageUrl) $ toHtml $ renderDisplayUrl imageUrl
          when (not (L.null (entryVideoUrls e))) $ do
            forM_ (entryVideoUrls e) $ \videoUrl -> do
              H.a ! A.href (urlValue videoUrl) $ toHtml $ renderDisplayUrl videoUrl
          when (not (L.null (entryAudioUrls e))) $ do
            forM_ (entryAudioUrls e) $ \audioUrl -> do
              H.a ! A.href (urlValue audioUrl) $ toHtml $ renderDisplayUrl audioUrl
