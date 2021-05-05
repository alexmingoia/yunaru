{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Feed where

import App.Controller.NotFound as NotFound
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.View.Feed
import App.View.Language
import App.View.Page
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai.Responder

pageSize = 30

get :: Text -> Responder AppEnv IO a
get urlP = do
  env <- getEnv
  url <- maybe NotFound.get pure (parseUrl urlP)
  userM <- Session.getUser
  feedDtld <- maybe NotFound.get pure =<< DB.exec (FeedDetailed.findOne url userM)
  let feed = feedInfo feedDtld
  beforeM <- (parseDateTime =<<) <$> getQueryParam "before"
  entriesDtld <- DB.exec (EntryDetailed.find pageSize beforeM (feedUrl feed))
  now <- liftIO getCurrentTime
  sendHtmlPage status200
    $ withLocation (FeedLocation feedDtld)
    $ withHead (feedMetaTags env feedDtld)
    $ withClass "h-feed"
    $ withHtml
    $ feedHtml env now feedDtld entriesDtld beforeM pageSize
