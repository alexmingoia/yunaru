{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Author where

import App.Model.Database as DB
import App.Model.Env
import App.Model.Feed as Feed
import App.View.URL
import Data.List as L
import Web.Twain

get :: RouteM AppEnv a
get = do
  url <- param "url"
  feeds <- DB.exec (Feed.find url)
  -- Author's default feed is the first feed matching the author's URL host.
  let authorFeeds =
        L.sortOn (not . (== urlAuthority url) . urlAuthority . feedUrl) feeds
  -- Redirect to author's default feed if they have one, otherwise redirect to author's URL.
  case authorFeeds of
    (feed : _) -> send $ redirect302 $ renderUrl (localFeedUrl feed)
    _ -> send $ redirect302 $ renderUrl url
