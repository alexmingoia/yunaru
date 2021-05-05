{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Author where

import App.Controller.NotFound as NotFound
import App.Model.Database as DB
import App.Model.Env
import App.Model.Feed as Feed
import App.View.URL
import Data.List as L
import Data.Maybe
import Network.Wai.Responder

get :: Text -> Responder AppEnv IO a
get urlP = do
  env <- getEnv
  url <- maybe NotFound.get pure (parseUrl urlP)
  feeds <- DB.exec (Feed.find url)
  -- Author's default feed is the first feed matching the author's URL host.
  let defaultFeedM = listToMaybe (L.sortOn (not . (== urlAuthority url) . urlAuthority . feedUrl) feeds)
  -- Redirect to author's default feed if they have one, otherwise redirect to author's URL.
  case defaultFeedM of
    Nothing -> send (redirect302 urlP)
    Just feed -> send (redirect302 (renderUrl (localFeedUrl env feed)))
