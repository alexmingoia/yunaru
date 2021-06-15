{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.Entry where

import App.Model.Author
import App.Model.Feed
import App.Model.Selda
import App.Model.URL
import Control.Applicative
import Data.List as L
import Data.Maybe
import Text.HTML.TagSoup as TS

data Entry
  = Entry
      { entryUrl :: URL,
        entryAuthorUrl :: URL,
        entryFeedUrl :: URL,
        entryName :: Maybe Text,
        entrySummary :: Maybe Text,
        entryContent :: Maybe Text,
        entryImageUrls :: [URL],
        entryAudioUrls :: [URL],
        entryVideoUrls :: [URL],
        entryRebloggedBy :: Maybe URL,
        entryRebloggedAt :: Maybe UTCTime,
        entryPublishedAt :: Maybe UTCTime,
        entryUpdatedAt :: Maybe UTCTime,
        entryImportedAt :: Maybe UTCTime
      }
  deriving (Generic)

instance SqlRow Entry

instance Eq Entry where
  (==) a b = entryUrl a == entryUrl b && entryFeedUrl a == entryFeedUrl b

instance Ord Entry where
  compare a b =
    compare
      (entryRebloggedAt a <|> entryPublishedAt a <|> entryImportedAt a)
      (entryRebloggedAt b <|> entryPublishedAt b <|> entryImportedAt b)

entries :: Table Entry
entries = tableFieldMod "entries" [] (toFieldName "entry")

emptyEntry :: URL -> AuthorURL -> FeedURL -> Entry
emptyEntry url authorUrl feedUrl =
  Entry
    { entryUrl = url,
      entryAuthorUrl = authorUrl,
      entryFeedUrl = feedUrl,
      entryName = Nothing,
      entrySummary = Nothing,
      entryContent = Nothing,
      entryImageUrls = [],
      entryAudioUrls = [],
      entryVideoUrls = [],
      entryRebloggedBy = Nothing,
      entryRebloggedAt = Nothing,
      entryPublishedAt = Nothing,
      entryUpdatedAt = Nothing,
      entryImportedAt = Nothing
    }

entryRebloggedOrPublishedAt e = entryRebloggedAt e <|> entryPublishedAt e

contentUrls :: Entry -> [URL]
contentUrls e = maybe [] extractUrls (entryContent e)
  where
    extractUrls =
      catMaybes . (fmap (parseInputUrl . TS.fromAttrib "href")) . L.filter (TS.isTagOpenName "a") . TS.parseTags

findOne :: URL -> URL -> SeldaT PG IO (Maybe Entry)
findOne feedUrl url = fmap listToMaybe <$> query $ do
  e <- select entries
  restrict (e ! #entryUrl .== literal url .&& e ! #entryFeedUrl .== literal feedUrl)
  return e

save :: Entry -> SeldaT PG IO Entry
save entry = transaction $ do
  let httpUrl = withHttp $ entryUrl entry
      httpsUrl = withHttps $ entryUrl entry
  existingEntry <- query $ do
    e <- select entries
    restrict (e ! #entryUrl .== literal httpUrl .|| e ! #entryUrl .== literal httpsUrl)
    restrict (e ! #entryFeedUrl .== literal (entryFeedUrl entry))
    return e
  case listToMaybe existingEntry of
    Nothing -> insert_ entries [entry]
    Just ee ->
      update_
        entries
        (\e -> e ! #entryUrl .== literal (entryUrl ee) .&& e ! #entryFeedUrl .== literal (entryFeedUrl ee))
        ( \e ->
            e
              `with` [ #entryName := literal (entryName entry),
                       #entrySummary := literal (entrySummary entry),
                       #entryContent := literal (entryContent entry),
                       #entryImageUrls := literal (entryImageUrls entry),
                       #entryAudioUrls := literal (entryAudioUrls entry),
                       #entryVideoUrls := literal (entryVideoUrls entry),
                       #entryRebloggedBy := literal (entryRebloggedBy entry),
                       #entryRebloggedAt := literal (entryRebloggedAt entry),
                       #entryUpdatedAt := literal (entryUpdatedAt entry)
                     ]
        )
  return entry

delete :: Entry -> SeldaT PG IO ()
delete entry =
  deleteFrom_
    entries
    ( \p ->
        p ! #entryUrl .== literal (entryUrl entry)
          .&& p ! #entryFeedUrl .== literal (entryFeedUrl entry)
    )
