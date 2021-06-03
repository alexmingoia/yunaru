{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.Feed where

import App.Model.Author as Author
import App.Model.Error
import App.Model.Selda
import App.Model.URL
import Control.Monad
import Data.List as L
import Data.Maybe
import Data.Time.Clock

type FeedURL = URL

data Feed
  = Feed
      { feedUrl :: FeedURL,
        feedAuthorUrl :: AuthorURL,
        feedName :: Maybe Text,
        feedSummary :: Maybe Text,
        feedFormat :: FeedFormat,
        feedRecentEntryUrl :: Maybe URL,
        feedUpdatedAt :: Maybe UTCTime,
        feedImportedAt :: Maybe UTCTime,
        feedImportError :: Maybe ImportError,
        feedCreatedAt :: Maybe UTCTime
      }
  deriving (Generic, Show)

instance SqlRow Feed

instance Eq Feed where
  (==) a b = feedUrl a == feedUrl b

instance Ord Feed where
  compare a b = compare (feedUpdatedAt a) (feedUpdatedAt b)

feeds :: Table Feed
feeds = tableFieldMod "feeds" [] (toFieldName "feed")

data FeedFormat
  = MF2FeedFormat
  | RSSFeedFormat
  | AtomFeedFormat
  | XMLFeedFormat
  | TwitterFeedFormat
  | EmailFeedFormat
  deriving (Eq, Show, Read, Bounded, Enum)

instance SqlType FeedFormat

data Featured
  = Featured
      { featuredCategory :: Text,
        featuredFeedUrl :: URL,
        featuredSummary :: Maybe Text
      }
  deriving (Generic, Show)

instance SqlRow Featured

featuredTable :: Table Featured
featuredTable = tableFieldMod "featured" [] (toFieldName "featured")

feedCategories :: [Text]
feedCategories =
  [ "Art & Design",
    "Music",
    "Business & Marketing",
    "Technology",
    "Culture & Lifestyle",
    "Gaming",
    "Spirituality"
  ]

find :: AuthorURL -> SeldaT PG IO [Feed]
find url = query $ do
  f <- select feeds
  restrict (f ! #feedAuthorUrl .== literal url)
  return f

findOne :: FeedURL -> SeldaT PG IO (Maybe Feed)
findOne url = fmap listToMaybe <$> query $ do
  f <- select feeds
  restrict (f ! #feedUrl .== literal url)
  return f

existsByURL :: URL -> SeldaT PG IO Bool
existsByURL url = fmap (not . L.null) <$> query $ do
  f <- select feeds
  restrict (f ! #feedUrl .== literal url)
  return (f ! #feedUrl)

-- | Select and timestamp feeds due for import.
dequeueFeedsToImport :: SeldaT PG IO [Feed]
dequeueFeedsToImport = do
  now <- liftIO getCurrentTime
  let min10ago = just (literal (addUTCTime (-600) now))
      min30ago = just (literal (addUTCTime (-1800) now))
      hr1ago = just (literal (addUTCTime (-3600) now))
      hr2ago = just (literal (addUTCTime (-7200) now))
      hr4ago = just (literal (addUTCTime (-14400) now))
      day1ago = just (literal (addUTCTime (-86400) now))
      day2ago = just (literal (addUTCTime (-172800) now))
  transaction $ do
    res <- query $ limit 0 10 $ do
      f <- select feeds
      let backoff1 = f ! #feedUpdatedAt .>= min30ago .&& f ! #feedImportedAt .<= min10ago
          backoff2 = f ! #feedUpdatedAt .>= hr1ago .&& f ! #feedImportedAt .<= min30ago
          backoff3 = f ! #feedUpdatedAt .>= day1ago .&& f ! #feedImportedAt .<= hr1ago
          backoff4 = f ! #feedUpdatedAt .>= day2ago .&& f ! #feedImportedAt .<= hr2ago
          backoff5 = f ! #feedImportedAt .<= hr4ago
          noPublishDate = isNull (f ! #feedUpdatedAt) .&& f ! #feedImportedAt .<= hr4ago
          isImported = not_ (isNull (f ! #feedImportedAt))
      restrict (isImported .&& (noPublishDate .|| backoff1 .|| backoff2 .|| backoff3 .|| backoff4 .|| backoff5))
      order (f ! #feedImportedAt) ascending
      return f
    update_
      feeds
      (\f -> f ! #feedUrl `isIn` (literal . feedUrl <$> res))
      (\f -> f `with` [#feedImportedAt := just (literal now)])
    return res

save :: Feed -> SeldaT PG IO ()
save feed = transaction $ do
  updated <-
    update
      feeds
      (\f -> f ! #feedUrl .== literal (feedUrl feed))
      ( \f ->
          f
            `with` [ #feedAuthorUrl := literal (feedAuthorUrl feed),
                     #feedName := literal (feedName feed),
                     #feedSummary := literal (feedSummary feed),
                     #feedFormat := literal (feedFormat feed),
                     #feedRecentEntryUrl := literal (feedRecentEntryUrl feed),
                     #feedUpdatedAt := literal (feedUpdatedAt feed),
                     #feedImportedAt := literal (feedImportedAt feed)
                   ]
      )
  when (updated == 0) $ insert_ feeds [feed]

updateURL :: Feed -> URL -> SeldaT PG IO ()
updateURL feed url =
  update_
    feeds
    (\f -> f ! #feedUrl .== literal (feedUrl feed))
    ( \f ->
        f
          `with` [#feedUrl := literal url]
    )
