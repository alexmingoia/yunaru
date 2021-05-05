{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Entry where

import App.Controller.NotFound as NotFound
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.View.Entry
import App.View.Language
import App.View.Page
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai.Responder

pageSize = 30

get :: Text -> Text -> Responder AppEnv IO a
get feedUrlP entryUrlP = do
  now <- liftIO getCurrentTime
  feedUrl <- maybe NotFound.get pure (parseUrl feedUrlP)
  url <- maybe NotFound.get pure (parseUrl entryUrlP)
  entryDtld <- maybe NotFound.get pure =<< (DB.exec (EntryDetailed.findOne feedUrl url))
  env <- getEnv
  sendHtmlPage status200
    $ withLocation (EntryLocation entryDtld)
    $ withHead (entryOgLinks env entryDtld)
    $ withHtml
    $ entryHtml env now entryDtld

getFollowing :: Maybe AppError -> Responder AppEnv IO a
getFollowing err = do
  env <- getEnv
  beforeM <- (parseDateTime =<<) <$> getQueryParam "before"
  userM <- Session.getUser
  entriesDtld <- maybe (pure []) (DB.exec . EntryDetailed.findFollowing pageSize beforeM) userM
  now <- liftIO getCurrentTime
  sendHtmlPage (errorStatus err)
    $ withLocation UntitledLocation
    $ withHtml
    $ followingEntriesHtml env now userM pageSize beforeM entriesDtld
