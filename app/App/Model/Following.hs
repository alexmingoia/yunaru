{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.Following where

import App.Model.Selda
import App.Model.URL
import App.Model.User
import Control.Monad
import Control.Monad.Catch
import Data.List as L
import Data.Maybe
import qualified Data.Text as T

data Following
  = Following
      { followingUserId :: UUID,
        followingFeedUrl :: URL,
        followingMuted :: Bool
      }
  deriving (Eq, Generic)

instance SqlRow Following

followings :: Table Following
followings =
  tableFieldMod
    "followings"
    [#followingUserId :+ #followingFeedUrl :- primary]
    (toFieldName "following")

findOne :: User -> URL -> SeldaT PG IO (Maybe Following)
findOne user url = fmap listToMaybe <$> query $ do
  f <- select followings
  restrict (f ! #followingUserId .== literal (userId user) .&& f ! #followingFeedUrl .== literal url)
  return f

save :: Following -> SeldaT PG IO ()
save following = do
  let uid = followingUserId following
      feedUrl = followingFeedUrl following
  updated <-
    update
      followings
      (\f -> f ! #followingUserId .== literal uid .&& f ! #followingFeedUrl .== literal feedUrl)
      (\f -> f `with` [#followingMuted := literal (followingMuted following)])
  when (updated == 0) $ insert_ followings [following]

saveIfNew :: Following -> SeldaT PG IO ()
saveIfNew following = do
  new <- fmap L.null <$> query $ do
    f <- select followings
    restrict (f ! #followingUserId .== literal (followingUserId following) .&& f ! #followingFeedUrl .== literal (followingFeedUrl following))
    return (f ! #followingUserId)
  when new $ insert_ followings [following]

type ExistingUser = User

type NewUser = User

updateUser :: ExistingUser -> NewUser -> SeldaT PG IO ()
updateUser existing new = handle ignoreDupeError $ do
  update_
    followings
    (\f -> f ! #followingUserId .== literal (userId existing))
    (\f -> f `with` [#followingUserId := literal (userId new)])
  where
    ignoreDupeError err@(SqlError msg) = if "followings_pkey" `T.isInfixOf` T.pack msg then pure () else throwM err
    ignoreDupeError err = throwM err

delete :: Following -> SeldaT PG IO ()
delete following = do
  let uid = followingUserId following
      feedUrl = followingFeedUrl following
  deleteFrom_
    followings
    (\f -> f ! #followingUserId .== literal uid .&& f ! #followingFeedUrl .== literal feedUrl)
