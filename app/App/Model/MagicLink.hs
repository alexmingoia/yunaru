{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.MagicLink where

import App.Model.Database
import App.Model.Env
import Data.Maybe
import Data.Time.Clock

data MagicLink
  = MagicLink
      { magicLinkId :: UUID,
        magicLinkEmail :: Email,
        magicLinkExpiresAt :: UTCTime,
        magicLinkAction :: MagicLinkAction,
        magicLinkUserId :: Maybe UUID,
        magicLinkSentAt :: Maybe UTCTime
      }
  deriving (Generic)

instance SqlRow MagicLink

magicLinks :: Table MagicLink
magicLinks = tableFieldMod "magic_links" [#magicLinkId :- primary] (toFieldName "magicLink")

data MagicLinkAction = SignInMagicLink | VerifyEmailMagicLink
  deriving (Eq, Show, Read, Bounded, Enum)

instance SqlType MagicLinkAction

findOne :: UUID -> SeldaT PG IO (Maybe MagicLink)
findOne mid = fmap listToMaybe <$> query $ do
  m <- select magicLinks
  restrict (m ! #magicLinkId .== literal mid)
  return m

findOneVerifyEmail :: UUID -> SeldaT PG IO (Maybe Email)
findOneVerifyEmail uid = fmap listToMaybe <$> query $ do
  m <- select magicLinks
  restrict (m ! #magicLinkUserId .== literal (Just uid) .&& m ! #magicLinkAction .== literal VerifyEmailMagicLink)
  return (m ! #magicLinkEmail)

save :: MagicLink -> SeldaT PG IO ()
save magicLink = insert_ magicLinks [magicLink]

dequeueUnsentMagicLinks :: SeldaT PG IO [MagicLink]
dequeueUnsentMagicLinks = do
  now <- liftIO getCurrentTime
  transaction $ do
    ms <- query $ limit 0 10 $ do
      m <- select magicLinks
      restrict (isNull (m ! #magicLinkSentAt))
      order (m ! #magicLinkExpiresAt) ascending
      return m
    update_
      magicLinks
      (\m -> m ! #magicLinkId `isIn` (literal . magicLinkId . first <$> ms))
      (\m -> m `with` [#magicLinkSentAt := literal (Just now)])
    return ms

delete :: MagicLink -> SeldaT PG IO ()
delete magicLink = deleteFrom_ magicLinks (\m -> m ! #magicLinkId .== literal (magicLinkId magicLink))

deleteExpired :: SeldaT PG IO ()
deleteExpired = do
  now <- liftIO getCurrentTime
  deleteFrom_ magicLinks (\m -> m ! #magicLinkExpiresAt .< literal now)
