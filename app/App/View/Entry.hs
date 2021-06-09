{-# LANGUAGE OverloadedStrings #-}

module App.View.Entry where

import App.Model.Author
import App.Model.EntryDetailed
import App.Model.Env
import App.Model.Image as Image
import App.Model.Twitter as Twitter
import App.View.Icon as Icon
import App.View.Language
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
    whenJust (entryRebloggedOrPublishedAt entry) $ \ts -> do
      H.span " • "
      H.a ! A.href (urlValue (canonicalEntryUrl entry)) $ do
        H.time
          ! A.class_ "dt-published"
          ! A.datetime (textValue (formatTime8601 ts))
          ! A.pubdate mempty
          ! A.title (textValue (formatTimeHuman ts))
          $ toHtml (formatTimeAgoCompact now ts)

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

entryHtml now entryDtld = do
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
