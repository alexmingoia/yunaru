{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Entry where

import App.Controller.NotFound as NotFound
import App.Controller.Page
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.View.Entry
import App.View.Following
import App.View.Language
import Data.List as L
import Data.Time.Clock
import Network.HTTP.Types
import System.Random.Shuffle
import Web.Twain

pageSize = 30

get :: RouteM AppEnv a
get = do
  feedUrlP <- param "feedUrl"
  entryUrlP <- param "entryUrl"
  now <- liftIO getCurrentTime
  feedUrl <- maybe NotFound.get pure (parseUrl feedUrlP)
  url <- maybe NotFound.get pure (parseUrl entryUrlP)
  entryDtld <- maybe NotFound.get pure =<< (DB.exec (EntryDetailed.findOne feedUrl url))
  sendHtmlPage status200 (entryPageTitle entryDtld) $ entryHtml now entryDtld

getFollowing :: Maybe AppError -> RouteM AppEnv a
getFollowing err = do
  appEnv <- env
  beforeM <- (parseDateTime =<<) <$> paramMaybe "before"
  categoryM <- paramMaybe "category"
  userM <- Session.getUser
  entriesDtld <- maybe (pure []) (DB.exec . EntryDetailed.findFollowing pageSize beforeM) userM
  feedsDtld <-
    if L.null entriesDtld
      then do
        fds <- DB.exec $ FeedDetailed.findByCategory categoryM userM
        liftIO $ shuffleM fds
      else pure []
  now <- liftIO getCurrentTime
  sendHtmlPage (errorStatus err) (appName appEnv) $
    followingEntriesHtml appEnv now pageSize beforeM categoryM entriesDtld feedsDtld
