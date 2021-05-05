{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.FollowingDetailed
  ( module App.Model.FollowingDetailed,
    Following (..),
    Feed (..),
    Author (..),
    Entry (..),
  )
where

import App.Model.Author
import App.Model.Database
import App.Model.Entry
import App.Model.Feed
import App.Model.Following
import App.Model.User
import Control.Monad.Extra (whenJust)
import Data.List

data FollowingDetailed
  = FollowingDetailed
      { followingInfo :: Following,
        followingFeed :: Feed,
        followingAuthor :: Author,
        followingRecentEntry :: Maybe Entry
      }

instance Eq FollowingDetailed where
  (==) a b = (==) (followingInfo a) (followingInfo b)

instance Ord FollowingDetailed where
  compare a b = compare (followingFeed a) (followingFeed b)

find :: Int -> Maybe UTCTime -> UserID -> SeldaT PG IO [FollowingDetailed]
find pageSize beforeM uid = do
  results <- query $ limit 0 pageSize $ do
    fg <- select followings
    restrict (fg ! #followingUserId .== literal uid)
    fd <- innerJoin (\fd -> fd ! #feedUrl .== fg ! #followingFeedUrl) $ select feeds
    fa <- innerJoin (\fa -> fa ! #authorUrl .== fd ! #feedAuthorUrl) $ select authors
    e <-
      leftJoin
        (\e -> e ! #entryFeedUrl .== fd ! #feedUrl .&& just (e ! #entryUrl) .== fd ! #feedRecentEntryUrl)
        $ select entries
    whenJust beforeM $ \before ->
      restrict
        ( not_ (isNull (fd ! #feedUpdatedAt))
            .&& fd ! #feedUpdatedAt .< literal (Just before)
        )
    order (fd ! #feedImportedAt) descending
    order (fd ! #feedUpdatedAt) descending
    order (isNull (fd ! #feedUpdatedAt)) ascending
    return (fg :*: fd :*: fa :*: e)
  let followingsDtld = (\r -> FollowingDetailed (first r) (second r) (third r) (fourth r)) <$> results
      nubber a b = feedUrl (followingFeed a) == feedUrl (followingFeed b)
      deduped = nubBy nubber followingsDtld
  return deduped
