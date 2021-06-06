{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module App.Model.EntryDetailed
  ( module App.Model.EntryDetailed,
    Entry (..),
    emptyEntry,
    entryRebloggedOrPublishedAt,
    Author (..),
    emptyAuthor,
  )
where

import App.Model.Author as Author
import App.Model.Entry as Entry
import App.Model.Feed
import App.Model.Following
import App.Model.Selda
import App.Model.URL
import App.Model.User
import Control.Monad.Extra
import Data.Foldable
import qualified Data.List as L
import Data.Maybe

data EntryDetailed
  = EntryDetailed
      { entryInfo :: Entry,
        entryAuthor :: Author,
        entryReblogAuthor :: Maybe Author
      }

instance Eq EntryDetailed where
  (==) a b = entryInfo a == entryInfo b

instance Ord EntryDetailed where
  compare a b = compare (entryInfo a) (entryInfo b)

find :: Int -> Maybe UTCTime -> FeedURL -> SeldaT PG IO [EntryDetailed]
find pageSize beforeM url = do
  rows <- query $ limit 0 pageSize $ do
    e <- select entries
    restrict (e ! #entryFeedUrl .== literal url)
    whenJust beforeM $ \before ->
      restrict
        ( (not_ (isNull (e ! #entryRebloggedAt)) .&& e ! #entryRebloggedAt .< literal (Just before))
            .|| (isNull (e ! #entryRebloggedAt) .&& e ! #entryPublishedAt .< literal (Just before))
        )
    a <- innerJoin (\a -> a ! #authorUrl .== e ! #entryAuthorUrl) $ select authors
    ra <- leftJoin (\ra -> just (ra ! #authorUrl) .== e ! #entryRebloggedBy) $ select authors
    order (e ! #entryImportedAt) descending
    order (ifThenElse (isNull (e ! #entryRebloggedAt)) (e ! #entryPublishedAt) (e ! #entryRebloggedAt)) descending
    return (e :*: a :*: ra)
  return (toEntryDetailed <$> rows)
  where
    toEntryDetailed r = EntryDetailed (first r) (second r) (third r)

findFollowing :: Int -> Maybe UTCTime -> User -> SeldaT PG IO [EntryDetailed]
findFollowing pageSize beforeM user = do
  rows <- query $ limit 0 pageSize $ do
    fg <- select followings
    restrict (fg ! #followingUserId .== literal (userId user) .&& fg ! #followingMuted .== false)
    e <- innerJoin (\e -> e ! #entryFeedUrl .== fg ! #followingFeedUrl) (select entries)
    whenJust beforeM $ \before ->
      restrict
        ( (not_ (isNull (e ! #entryRebloggedAt)) .&& e ! #entryRebloggedAt .< literal (Just before))
            .|| (isNull (e ! #entryRebloggedAt) .&& e ! #entryPublishedAt .< literal (Just before))
        )
    restrict (not_ (isNull (e ! #entryPublishedAt)))
    a <- innerJoin (\a -> a ! #authorUrl .== e ! #entryAuthorUrl) $ select authors
    ra <- leftJoin (\ra -> just (ra ! #authorUrl) .== e ! #entryRebloggedBy) $ select authors
    let publishedAt =
          ifThenElse
            (isNull (e ! #entryRebloggedAt))
            (e ! #entryPublishedAt)
            (e ! #entryRebloggedAt)
    order (e ! #entryImportedAt) descending
    order publishedAt descending
    return (e :*: a :*: ra)
  return (toEntryDetailed <$> rows)
  where
    toEntryDetailed r = EntryDetailed (first r) (second r) (third r)

findOne :: FeedURL -> URL -> SeldaT PG IO (Maybe EntryDetailed)
findOne feedUrl url = fmap (fmap toEntryDetailed . listToMaybe) <$> query $ do
  e <- select entries
  restrict (e ! #entryUrl .== literal url .&& e ! #entryFeedUrl .== literal feedUrl)
  a <- innerJoin (\a -> a ! #authorUrl .== e ! #entryAuthorUrl) (select authors)
  ra <- leftJoin (\ra -> just (ra ! #authorUrl) .== e ! #entryRebloggedBy) $ select authors
  return (e :*: a :*: ra)
  where
    toEntryDetailed r = EntryDetailed (first r) (second r) (third r)

saveAll :: [EntryDetailed] -> SeldaT PG IO ()
saveAll entriesDtld = do
  let as = L.nubBy (\a b -> authorUrl a == authorUrl b) (entryAuthor <$> entriesDtld)
      fs = entryInfo <$> entriesDtld
  for_ as Author.save
  for_ fs Entry.save
