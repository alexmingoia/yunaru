{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Feed where

import App.Controller.NotFound as NotFound
import App.Controller.Page
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.View.Feed
import App.View.Language
import Data.Time.Clock
import Network.HTTP.Types
import Web.Twain

pageSize = 30

get :: RouteM AppEnv a
get = do
  appEnv <- env
  urlP <- param "url"
  url <- maybe NotFound.get pure (parseUrl urlP)
  userM <- Session.getUser
  feedDtld <- maybe NotFound.get pure =<< DB.exec (FeedDetailed.findOne url userM)
  let feed = feedInfo feedDtld
  beforeM <- (parseDateTime =<<) <$> paramMaybe "before"
  entriesDtld <- DB.exec (EntryDetailed.find pageSize beforeM (feedUrl feed))
  now <- liftIO getCurrentTime
  sendHtmlPage status200 (feedPageTitle feedDtld) $
    feedHtml appEnv now feedDtld entriesDtld beforeM pageSize
