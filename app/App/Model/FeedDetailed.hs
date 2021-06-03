{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module App.Model.FeedDetailed
  ( module App.Model.FeedDetailed,
    Feed (..),
    FeedFormat (..),
  )
where

import App.Model.Author as Author
import App.Model.Entry as Entry
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Feed as Feed
import App.Model.Following
import App.Model.Selda
import App.Model.URL
import App.Model.User
import Control.Applicative
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock

data FeedDetailed
  = FeedDetailed
      { feedInfo :: Feed,
        feedAuthor :: Author,
        feedFollowing :: Maybe Following,
        feedRecentEntry :: Maybe Entry
      }

instance Eq FeedDetailed where
  (==) a b = feedInfo a == feedInfo b

instance Ord FeedDetailed where
  compare a b = compare (feedInfo a) (feedInfo b)

ignoredFeedNames :: [Text]
ignoredFeedNames =
  [ "News",
    "Blog",
    "Latest"
  ]

feedDisplayName fd =
  let f = feedInfo fd
      a = feedAuthor fd
      displayUrl = renderDisplayUrl (feedUrl f)
   in if feedName f `L.elem` (Just <$> ignoredFeedNames)
        then fromMaybe displayUrl (authorName a)
        else fromMaybe displayUrl (feedName f <|> authorName a)

feedDetailed f a = FeedDetailed f a Nothing Nothing

findOne :: URL -> Maybe User -> SeldaT PG IO (Maybe FeedDetailed)
findOne url userM = do
  fmap (fmap toFeedDetailed . listToMaybe) <$> query $ do
    f <- select feeds
    restrict (f ! #feedUrl .== literal url)
    a <- innerJoin (\a -> a ! #authorUrl .== f ! #feedAuthorUrl) (select authors)
    fg <-
      leftJoin
        ( maybe
            (const false)
            (\u -> \fg -> fg ! #followingUserId .== literal (userId u) .&& fg ! #followingFeedUrl .== f ! #feedUrl)
            userM
        )
        (select followings)
    return (f :*: a :*: fg)
  where
    toFeedDetailed r = FeedDetailed (first r) (second r) (third r) Nothing

findByCategory :: Maybe Text -> Maybe User -> SeldaT PG IO [FeedDetailed]
findByCategory categoryM userM = do
  fmap (fmap toFeedDetailed) <$> query $ distinct $ do
    f <- select feeds
    a <- innerJoin (\a -> a ! #authorUrl .== f ! #feedAuthorUrl) (select authors)
    fe <- case categoryM of
      Nothing -> do
        innerJoin
          (\c -> c ! #featuredFeedUrl .== f ! #feedUrl)
          $ select featuredTable
      Just category -> do
        innerJoin
          ( \c ->
              c ! #featuredCategory .== literal category
                .&& c ! #featuredFeedUrl .== f ! #feedUrl
          )
          $ select featuredTable
    e <-
      leftJoin
        (\e -> e ! #entryFeedUrl .== f ! #feedUrl .&& just (e ! #entryUrl) .== f ! #feedRecentEntryUrl)
        $ select entries
    fg <-
      leftJoin
        ( maybe
            (const false)
            (\u -> \fg -> fg ! #followingUserId .== literal (userId u) .&& fg ! #followingFeedUrl .== f ! #feedUrl)
            userM
        )
        (select followings)
    orderRandom
    return (f :*: a :*: (fe ! #featuredSummary) :*: e :*: fg)
  where
    toFeedDetailed r =
      let feed = first r
          author = second r
          featuredSummaryM = third r
          entryM = fourth r
          followingM = fifth r
          summary = featuredSummaryM <|> feedSummary feed
       in FeedDetailed (feed {feedSummary = summary}) author followingM entryM

-- | Insert or update feed author and entries.
save :: FeedDetailed -> SeldaT PG IO ()
save feedDtld = do
  let author = feedAuthor feedDtld
      feed = feedInfo feedDtld
  Author.save author
  Feed.save feed

timestampEntries :: FeedDetailed -> [EntryDetailed] -> IO (FeedDetailed, [EntryDetailed])
timestampEntries feedDtld entriesDtld = do
  now <- getCurrentTime
  let latestEntry = if L.null entriesDtld then Nothing else Just (entryInfo (L.maximum entriesDtld))
  return (stampFeedDetailed now latestEntry feedDtld, stampEntryDetailed now <$> entriesDtld)
  where
    stampFeedDetailed now latestEntry fd =
      fd
        { feedInfo =
            (feedInfo fd)
              { feedRecentEntryUrl = entryUrl <$> latestEntry,
                feedUpdatedAt = entryRebloggedOrPublishedAt =<< latestEntry,
                feedImportedAt = Just now,
                feedCreatedAt = feedCreatedAt (feedInfo fd) <|> Just now
              },
          feedAuthor = (feedAuthor fd) {authorImportedAt = Just now}
        }
    stampEntryDetailed now ed =
      ed
        { entryInfo = (entryInfo ed) {entryImportedAt = Just now},
          entryAuthor = (entryAuthor ed) {authorImportedAt = Just now}
        }
