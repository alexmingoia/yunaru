{-# LANGUAGE OverloadedStrings #-}

module App.View.URL where

import App.Model.Author
import App.Model.EntryDetailed
import App.Model.Env
import App.Model.Feed
import Data.Maybe
import Data.Text

localEntryUrl :: AppEnv -> Entry -> URL
localEntryUrl env e =
  appUrl env +> ["feeds", renderUrl (entryFeedUrl e), "entries", renderUrl (entryUrl e)]

canonicalEntryUrl :: AppEnv -> Entry -> URL
canonicalEntryUrl env e =
  if isRelative (entryUrl e)
    then canonicalEntryFeedUrl env e +> ["entries", renderUrl (entryUrl e)]
    else entryUrl e

localEntryFeedUrl :: AppEnv -> Entry -> URL
localEntryFeedUrl env e = appUrl env +> ["feeds", renderUrl (entryFeedUrl e)]

canonicalEntryFeedUrl :: AppEnv -> Entry -> URL
canonicalEntryFeedUrl env e =
  if isRelative (entryFeedUrl e)
    then localEntryFeedUrl env e
    else entryFeedUrl e

localEntryAuthorUrl :: AppEnv -> EntryDetailed -> URL
localEntryAuthorUrl env entryDtld =
  let e = entryInfo entryDtld
   in if isJust (entryRebloggedBy e)
        then appUrl env +> ["authors", renderUrl (entryAuthorUrl e)]
        else localEntryFeedUrl env e

canonicalEntryAuthorUrl :: AppEnv -> EntryDetailed -> URL
canonicalEntryAuthorUrl env entryDtld =
  let e = entryInfo entryDtld
      a = entryAuthor entryDtld
   in if entryFeedUrl e == authorUrl a
        then canonicalEntryFeedUrl env e
        else authorUrl a

localAuthorUrl :: AppEnv -> Author -> URL
localAuthorUrl e a = appUrl e +> ["authors", renderUrl (authorUrl a)]

canonicalAuthorUrl :: AppEnv -> Author -> URL
canonicalAuthorUrl e a =
  if isRelative (authorUrl a)
    then localAuthorUrl e a
    else authorUrl a

authorDisplayName :: AppEnv -> Author -> Text
authorDisplayName e a = fromMaybe (renderDisplayUrl (canonicalAuthorUrl e a)) (authorName a)

localFeedUrl :: AppEnv -> Feed -> URL
localFeedUrl env f = appUrl env +> ["feeds", renderUrl (feedUrl f)]

canonicalFeedUrl :: AppEnv -> Feed -> URL
canonicalFeedUrl env f =
  if isRelative (feedUrl f)
    then localFeedUrl env f
    else feedUrl f
