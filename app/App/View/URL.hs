{-# LANGUAGE OverloadedStrings #-}

module App.View.URL where

import App.Model.Author
import App.Model.EntryDetailed
import App.Model.Env
import App.Model.Feed
import Data.Maybe
import Data.Text

localEntryUrl :: Entry -> URL
localEntryUrl e =
  rootUrl
    +> [ "feeds",
         renderUrl (entryFeedUrl e),
         "entries",
         renderUrl (entryUrl e)
       ]

canonicalEntryUrl :: Entry -> URL
canonicalEntryUrl e =
  if isRelative (entryUrl e)
    then canonicalEntryFeedUrl e +> ["entries", renderUrl (entryUrl e)]
    else entryUrl e

localEntryFeedUrl :: Entry -> URL
localEntryFeedUrl e = rootUrl +> ["feeds", renderUrl (entryFeedUrl e)]

canonicalEntryFeedUrl :: Entry -> URL
canonicalEntryFeedUrl e =
  if isRelative (entryFeedUrl e)
    then localEntryFeedUrl e
    else entryFeedUrl e

localEntryAuthorUrl :: EntryDetailed -> URL
localEntryAuthorUrl entryDtld =
  let e = entryInfo entryDtld
   in if isJust (entryRebloggedBy e)
        then rootUrl +> ["authors", renderUrl (entryAuthorUrl e)]
        else localEntryFeedUrl e

canonicalEntryAuthorUrl :: EntryDetailed -> URL
canonicalEntryAuthorUrl entryDtld =
  let e = entryInfo entryDtld
      a = entryAuthor entryDtld
   in if entryFeedUrl e == authorUrl a
        then canonicalEntryFeedUrl e
        else authorUrl a

localAuthorUrl :: Author -> URL
localAuthorUrl a = rootUrl +> ["authors", renderUrl (authorUrl a)]

canonicalAuthorUrl :: Author -> URL
canonicalAuthorUrl a =
  if isRelative (authorUrl a)
    then localAuthorUrl a
    else authorUrl a

localFeedUrl :: Feed -> URL
localFeedUrl f = rootUrl +> ["feeds", renderUrl (feedUrl f)]

canonicalFeedUrl :: Feed -> URL
canonicalFeedUrl f =
  if isRelative (feedUrl f)
    then localFeedUrl f
    else feedUrl f

authorDisplayName :: Author -> Text
authorDisplayName a =
  fromMaybe (renderDisplayUrl (canonicalAuthorUrl a)) (authorName a)
