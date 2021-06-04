{-# LANGUAGE OverloadedStrings #-}

module App.Model.RemoteFeed where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed
import App.Model.HTTP as HTTP
import App.Model.Microformats2 as Microformats2
import App.Model.RSS as RSS
import App.Model.Twitter as Twitter
import Control.Applicative
import Control.Exception
import Data.Time.Clock

importEntries ::
  AppEnv ->
  URL ->
  Maybe FeedFormat ->
  Maybe Author ->
  IO (FeedDetailed, [EntryDetailed])
importEntries env url (Just TwitterFeedFormat) _ =
  Twitter.importFeedEntries env url
importEntries env url (Just MF2FeedFormat) _ =
  Microformats2.importFeedEntries env url
importEntries env url (Just EmailFeedFormat) _ =
  Microformats2.importFeedEntries env url
importEntries env url (Just _) authorM = do
  case authorM of
    Nothing -> RSS.importFeedEntries env authorM url
    Just author -> do
      now <- getCurrentTime
      let oneWeekAgo = addUTCTime (-604800) now
      if authorImportedAt author < Just oneWeekAgo
        then do
          (_, body, finalUrl) <- HTTP.fetchAndRetryUrl (authorUrl author)
          let htmlAuthorM = Microformats2.authorFromHtml finalUrl body
          RSS.importFeedEntries env htmlAuthorM url
        else RSS.importFeedEntries env authorM url
importEntries env url Nothing authorM =
  if Twitter.isProfileUrl url
    then Twitter.importFeedEntries env url
    else do
      (res, body, finalUrl) <- HTTP.fetchAndRetryUrl url
      case contentType res of
        UnsupportedContentType -> throwIO (ImportError url FormatNotSupported)
        XMLContentType -> RSS.parseFeedEntries env authorM finalUrl body
        HTMLContentType -> do
          feedOrErr <- try $ Microformats2.parseFeedEntries env finalUrl body
          case feedOrErr of
            Left (ImportError _ _) -> do
              -- Follow any rel="alternative" XML feed URL.
              -- If no <link> found, try /feed (default RSS URL for WordPress)
              let xmlFeedUrlM =
                    Microformats2.extractXmlFeedUrl finalUrl body
                      <|> Just (finalUrl +> ["feed"])
              case xmlFeedUrlM of
                Nothing -> throwIO (ImportError url NoFeedFound)
                Just xmlFeedUrl -> do
                  let htmlAuthorM = Microformats2.authorFromHtml finalUrl body
                  RSS.importFeedEntries env htmlAuthorM xmlFeedUrl
            Left err -> throwIO err
            Right feed -> return feed
