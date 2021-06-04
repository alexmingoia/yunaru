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
                     #feedImportError := literal (feedImportError feed),
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
