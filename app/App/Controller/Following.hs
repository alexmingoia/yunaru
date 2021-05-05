{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Following where

import App.Controller.NotFound as NotFound
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.Model.Feed as Feed
import App.Model.FeedDetailed as FeedDetailed
import App.Model.Following as Following
import App.Model.FollowingDetailed as FollowingDetailed
import App.Model.RemoteFeed as RemoteFeed
import App.View.Following
import App.View.Language
import App.View.Page
import Control.Exception
import Data.Maybe
import Data.Time.Clock
import Network.Wai.Responder

pageSize = 50

getRecentEntryList :: Maybe AppError -> Responder AppEnv IO a
getRecentEntryList err = do
  env <- getEnv
  urlP <- fromMaybe "" <$> getParam "url"
  beforeM <- (parseDateTime =<<) <$> getQueryParam "before"
  userM <- Session.getUser
  followingsDtld <- maybe (pure []) (DB.exec . FollowingDetailed.find pageSize beforeM . userId) userM
  now <- liftIO getCurrentTime
  sendHtmlPage (errorStatus err)
    $ withLocation (TitledLocation "Following")
    $ withHtml
    $ followingsRecentEntryHtml env now userM pageSize beforeM err urlP followingsDtld

post :: Responder AppEnv IO a
post = do
  env <- getEnv
  user <- Session.getOrCreateUser
  let missingUrlError = InputError "url" "Missing parameter: url"
      invalidUrlError = InputError "url" "The URL you entered is not valid."
  urlP <- maybe (getRecentEntryList (Just missingUrlError)) pure =<< getParam "url"
  url <- maybe (getRecentEntryList (Just invalidUrlError)) pure (parseInputUrl urlP)
  let remoteFeed = RemoteFeed.fromUrl url
  (feedDtld, entriesDtld) <- either (getRecentEntryList . Just) pure =<< liftIO (try (RemoteFeed.importEntries env remoteFeed))
  DB.exec (FeedDetailed.save feedDtld)
  DB.exec (EntryDetailed.saveAll entriesDtld)
  let following =
        Following
          { followingUserId = userId user,
            followingFeedUrl = feedUrl (feedInfo feedDtld),
            followingMuted = False
          }
  DB.exec (Following.save following)
  sessionCookie <- Session.createSessionCookie user
  let redirectUrl = appUrl env +> ["followings"]
  send (setCookie sessionCookie (redirect303 (renderUrl redirectUrl)))

put :: Text -> Responder AppEnv IO a
put feedUrlP = do
  env <- getEnv
  user <- Session.getOrCreateUser
  url <- maybe NotFound.get pure (parseUrl feedUrlP)
  maybe NotFound.get pure =<< DB.exec (Feed.findOne url)
  isMuted <- maybe (pure False) (pure . (== "True")) =<< getParam "muted"
  let following = Following {followingUserId = userId user, followingFeedUrl = url, followingMuted = isMuted}
  DB.exec (Following.save following)
  redirectUrlM <- (parseUrl =<<) <$> getParam "redirect_url"
  sessionCookie <- Session.createSessionCookie user
  let to = fromMaybe (appUrl env +> ["feeds", renderUrl url]) redirectUrlM
  send (setCookie sessionCookie (redirect303 (renderUrl to)))

delete :: Text -> Responder AppEnv IO a
delete feedUrlP = do
  env <- getEnv
  user <- Session.requireUser
  url <- maybe NotFound.get pure (parseUrl feedUrlP)
  maybe NotFound.get pure =<< DB.exec (Feed.findOne url)
  let following = Following {followingUserId = userId user, followingFeedUrl = url, followingMuted = False}
  DB.exec (Following.delete following)
  redirectUrlM <- (parseUrl =<<) <$> getParam "redirect_url"
  sessionCookie <- Session.createSessionCookie user
  let to = fromMaybe (appUrl env +> ["feeds", renderUrl url]) redirectUrlM
  send (setCookie sessionCookie (redirect303 (renderUrl to)))
