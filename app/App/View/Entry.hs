{-# LANGUAGE OverloadedStrings #-}

module App.View.Entry where

import App.Model.Author
import App.Model.EntryDetailed
import App.Model.Env
import App.Model.Image as Image
import App.Model.Twitter as Twitter
import App.View.Icon as Icon
import App.View.Language
import App.View.Payment
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

followingEntriesHtml env now userM pageSize beforeM entriesDtld = do
  when (isNothing beforeM && L.null entriesDtld) noFollowingEntriesNoticeHtml
  H.div ! A.class_ "h-feed" $ do
    forM_ entriesDtld (entrySnippetHtml env now)
    when (isJust beforeM && pageSize /= L.length entriesDtld) endOfEntriesNoticeHtml
    when (pageSize == L.length entriesDtld) $ do
      let leastPublishedAtM = entryRebloggedOrPublishedAt (entryInfo (L.minimum entriesDtld))
      whenJust leastPublishedAtM $ \leastPublishedAt -> do
        let nextPageUrl = rootUrl ?> [("before", formatTime8601 leastPublishedAt)]
        H.nav $ H.small $ do
          H.a
            ! A.href (urlValue nextPageUrl)
            ! A.class_ "button"
            $ "More entries →"
  signupNoticeHtml env userM

endOfEntriesNoticeHtml = do
  H.p $ "You've reached the end of your feed. This may be a good time for a break."

noFollowingEntriesNoticeHtml = do
  H.h1 "A peaceful news feed."
  H.p "Follow RSS, Twitter, newsletters, and more. No ads, algorithm, or distractions."
  H.form ! A.method "POST" ! A.action "/followings" $ do
    H.div ! A.class_ "form-controls-inline" $ do
      H.div ! A.class_ "form-control" $ do
        H.label ! A.class_ "hidden" ! A.for "url" $ "URL to follow"
        H.input
          ! A.name "url"
          ! A.type_ "text"
          ! A.required mempty
          ! A.placeholder "Blog / RSS / Twitter / Tumblr / YouTube"
          ! A.class_ "input"
      H.div ! A.class_ "form-control" $ do
        H.button ! A.type_ "submit" $ "Follow"

entryBylineHtml now entryDtld = do
  let entry = entryInfo entryDtld
      author = entryAuthor entryDtld
      authorHref = localEntryAuthorUrl entryDtld
  H.div ! A.class_ "byline p-author h-card" $ do
    H.a
      ! A.href (urlValue authorHref)
      ! A.class_ "p-name"
      $ toHtml (authorDisplayName author)
    whenJust (entryReblogAuthor entryDtld) $ \a -> do
      let reblogName = fromMaybe (renderDisplayUrl (authorUrl a)) (authorName a)
          reblogHref = localAuthorUrl a
      toHtml (" " :: Text)
      Icon.reblog ! customAttribute "aria-label" "reblogged by"
      toHtml (" " :: Text)
      H.a
        ! A.href (urlValue reblogHref)
        ! A.class_ "reblogged-by"
        $ toHtml reblogName
    whenJust (entryPublishedAt entry) $ \publishedAt -> do
      H.span " • "
      H.a ! A.href (urlValue (canonicalEntryUrl entry)) $ do
        H.time
          ! A.class_ "dt-published"
          ! A.datetime (textValue (formatTime8601 publishedAt))
          ! A.pubdate mempty
          ! A.title (textValue (formatTimeHuman publishedAt))
          $ toHtml (formatTimeAgoCompact now publishedAt)

entrySnippetHtml env now entryDtld = do
  let entry = entryInfo entryDtld
      nameOrContentM =
        (Left <$> entryName entry) <|> (Right <$> entryContent entry)
          <|> (Right <$> entrySummary entry)
      isCite = isJust (entryReblogAuthor entryDtld)
  H.article ! A.class_ "h-entry" $ do
    H.div ! A.class_ (if isCite then "h-cite u-repost-of" else "h-cite") $ do
      whenJust nameOrContentM $ \nameOrContent -> case nameOrContent of
        Left name -> do
          H.h1 ! A.class_ "p-name" $ do
            H.a
              ! A.class_ "u-url"
              ! A.href (urlValue (canonicalEntryUrl entry))
              $ toHtml name
          entryBylineHtml now entryDtld
          whenJust (entrySummary entry) $ \summary -> do
            H.p ! A.class_ "p-summary" $ do
              H.a
                ! A.href (urlValue (localEntryUrl entry))
                $ preEscapedToHtml summary
        Right content -> do
          entryBylineHtml now entryDtld
          H.div ! A.class_ "e-content" $ preEscapedToHtml content
      videoHtml env entry
      imagesHtml env entry
      audioHtml entry

entryHtml env now entryDtld = do
  let entry = entryInfo entryDtld
  H.article ! A.class_ "h-entry" $ do
    whenJust (entryName entry) $ \name -> do
      H.h1 ! A.class_ "p-name" $ do
        H.a ! A.class_ "u-url" ! A.href (urlValue (canonicalEntryUrl entry)) $ toHtml name
    entryBylineHtml now entryDtld
    whenJust (entryContent entry) $ \content -> do
      H.div ! A.class_ "e-content" $ preEscapedToHtml content
    H.hr
    externalEntryNoticeHtml entryDtld

externalEntryNoticeHtml entryDtld = do
  let entry = entryInfo entryDtld
      href = canonicalEntryUrl entry
  when (isJust (entryImportedAt entry)) $ do
    H.p $ H.small $ do
      toHtml ("This entry originally appeared at " :: Text)
      H.a ! A.href (urlValue href) $ toHtml $ renderDisplayUrl href
      toHtml (", and may be a summary or abridged version." :: Text)

imagesHtml env entry = do
  when (not (L.null (entryImageUrls entry))) $ do
    H.div ! A.class_ "media images" $ forM_ (entryImageUrls entry) $ \imageUrl -> do
      H.a
        ! A.class_ "image"
        ! A.href (urlValue imageUrl)
        $ H.img ! A.src (urlValue (Image.cacheUrl env 800 800 False imageUrl)) ! A.class_ "u-photo"

videoHtml env entry = do
  when (not (L.null (entryVideoUrls entry))) $ do
    H.div ! A.class_ "media video" $ forM_ (entryVideoUrls entry) $ \videoUrl -> do
      if Twitter.isTwitterProfileUrl (entryFeedUrl entry)
        then do
          -- Twitter doesn't provide raw video links, only video preview images.
          H.a
            ! A.class_ "image video-preview"
            ! A.href (urlValue (canonicalEntryUrl entry))
            $ do
              Icon.play ! customAttribute "aria-hidden" "true"
              H.img ! A.src (urlValue (Image.cacheUrl env 800 800 False videoUrl))
        else do
          H.video ! A.src (urlValue videoUrl) ! A.controls mempty ! A.class_ "u-video" $ do
            H.a ! A.href (urlValue videoUrl) $ "Download video"

audioHtml entry = do
  when (not (L.null (entryAudioUrls entry))) $ do
    H.div ! A.class_ "media audio" $ forM_ (entryAudioUrls entry) $ \audioUrl -> do
      H.audio ! A.src (urlValue audioUrl) ! A.controls mempty ! A.class_ "u-audio" $ do
        H.a ! A.href (urlValue audioUrl) $ "Download audio"
