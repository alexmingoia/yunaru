{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Following where

import App.Controller.NotFound as NotFound
import App.Controller.Page
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.Model.Feed as Feed
import App.Model.FeedDetailed as FeedDetailed
import App.Model.Following as Following
import App.Model.FollowingDetailed as FollowingDetailed
import App.Model.RemoteFeed as RemoteFeed
import App.Model.User as User
import App.View.Following
import App.View.Language
import Control.Exception
import Data.Maybe
import Data.Time.Clock
import Web.Twain

pageSize = 50

getRecentEntryList :: Maybe AppError -> RouteM AppEnv a
getRecentEntryList err = do
  appEnv <- env
  urlP <- fromMaybe "" <$> paramMaybe "url"
  beforeM <- (parseDateTime =<<) <$> paramMaybe "before"
  userM <- Session.getUser
  followingsDtld <-
    maybe
      (pure [])
      (DB.exec . FollowingDetailed.find pageSize beforeM . userId)
      userM
  now <- liftIO getCurrentTime
  sendHtmlPage (errorStatus err) "Following" $
    followingsRecentEntryHtml appEnv now pageSize beforeM userM err urlP followingsDtld

post :: RouteM AppEnv a
post = do
  appEnv <- env
  user <- Session.getOrCreateUser
  let missingUrlError = InputError "url" "Missing parameter: url"
      invalidUrlError = InputError "url" "The URL you entered is not valid."
  urlP <- maybe (getRecentEntryList (Just missingUrlError)) pure =<< paramMaybe "url"
  url <- maybe (getRecentEntryList (Just invalidUrlError)) pure (parseInputUrl urlP)
  (fd, eds) <-
    either (getRecentEntryList . Just) pure
      =<< liftIO (try (RemoteFeed.importFromUrl appEnv url))
  DB.exec (FeedDetailed.save fd)
  DB.exec (EntryDetailed.saveAll eds)
  let following =
        Following
          { followingUserId = userId user,
            followingFeedUrl = feedUrl (feedInfo fd),
            followingMuted = False
          }
  DB.exec (Following.save following)
  sessionCookie <- Session.createSessionCookie user
  send $ withCookie' sessionCookie $ redirect303 "/followings"

put :: RouteM AppEnv a
put = do
  url <- param "url"
  user <- Session.getOrCreateUser
  maybe NotFound.get pure =<< DB.exec (Feed.findOne url)
  isMuted <-
    maybe (pure False) (pure . (== "True"))
      =<< (paramMaybe "muted" :: RouteM AppEnv (Maybe Text))
  let following =
        Following
          { followingUserId = userId user,
            followingFeedUrl = url,
            followingMuted = isMuted
          }
  DB.exec (Following.save following)
  redirectUrlM <- (parseUrl =<<) <$> paramMaybe "redirect_url"
  sessionCookie <- Session.createSessionCookie user
  let to = fromMaybe (rootUrl +> ["feeds", renderUrl url]) redirectUrlM
  send $ withCookie' sessionCookie $ redirect303 (renderUrl to)

delete :: RouteM AppEnv a
delete = do
  user <- Session.requireUser
  url <- param "url"
  maybe NotFound.get pure =<< DB.exec (Feed.findOne url)
  let following =
        Following
          { followingUserId = userId user,
            followingFeedUrl = url,
            followingMuted = False
          }
  DB.exec (Following.delete following)
  redirectUrlM <- (parseUrl =<<) <$> paramMaybe "redirect_url"
  sessionCookie <- Session.createSessionCookie user
  let to = fromMaybe (rootUrl +> ["feeds", renderUrl url]) redirectUrlM
  send $ withCookie' sessionCookie $ redirect303 (renderUrl to)
