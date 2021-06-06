{-# LANGUAGE OverloadedStrings #-}

module App.Model.RemoteFeed where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed
import App.Model.HTTP as HTTP
import App.Model.Microformats2 as Microformats2
import App.Model.RSS as RSS
import App.Model.Twitter as Twitter
import App.View.Language
import Control.Applicative
import Control.Exception
import Data.Maybe
import Data.Text.Encoding
import Data.Time.Clock
import Network.HTTP.Client hiding (parseUrl)
import Network.HTTP.Types
import Network.HTTP.Types.Header

updateFeed :: AppEnv -> FeedDetailed -> IO (FeedDetailed, [EntryDetailed])
updateFeed env feedDtld = do
  let feed = feedInfo feedDtld
      author = feedAuthor feedDtld
  case feedFormat feed of
    TwitterFeedFormat -> Twitter.importFeedEntries env (feedUrl feed)
    EmailFeedFormat -> return (feedDtld, [])
    MF2FeedFormat -> do
      (res, body, finalUrl) <- HTTP.fetchUrl (feedUrl feed) (feedHeaders feed)
      if responseStatus res == status304
        then do
          now <- getCurrentTime
          return (feedDtld {feedInfo = feed {feedImportedAt = Just now}}, [])
        else Microformats2.parseFeedEntries env finalUrl body
    _ -> do
      now <- getCurrentTime
      let oneWeekAgo = addUTCTime (-604800) now
      if authorImportedAt author < Just oneWeekAgo
        then do
          (_, body, url) <- HTTP.fetchUrl (authorUrl author) []
          let htmlAuthorM = Microformats2.authorFromHtml url body
              updatedAuthor =
                maybe
                  author
                  (\a -> a {authorImportedAt = Just now})
                  htmlAuthorM
          updateFeed env (feedDtld {feedAuthor = updatedAuthor})
        else do
          (res, body, url) <- HTTP.fetchUrl (feedUrl feed) (feedHeaders feed)
          if responseStatus res == status304
            then do
              return (feedDtld {feedInfo = feed {feedImportedAt = Just now}}, [])
            else RSS.parseFeedEntries env (Just author) url body

importFromUrl :: AppEnv -> URL -> IO (FeedDetailed, [EntryDetailed])
importFromUrl env url
  | Twitter.isProfileUrl url = Twitter.importFeedEntries env url
  | otherwise = do
    (res, body, finalUrl) <- HTTP.fetchUrl url []
    let etagM = extractETag res
    case contentType res of
      UnsupportedContentType -> throwIO (ImportError url FormatNotSupported)
      XMLContentType -> do
        (fd, eds) <- RSS.parseFeedEntries env Nothing finalUrl body
        return (fd {feedInfo = (feedInfo fd) {feedEtag = etagM}}, eds)
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
          Right (fd, eds) -> do
            return (fd {feedInfo = (feedInfo fd) {feedEtag = etagM}}, eds)

feedHeaders :: Feed -> [Header]
feedHeaders feed =
  let etagM = (\etag -> (hIfNoneMatch, encodeUtf8 etag)) <$> feedEtag feed
      updatedAtFmtM = formatTimeHttp <$> feedUpdatedAt feed
      updatedAtM = (\t -> (hIfModifiedSince, encodeUtf8 t)) <$> updatedAtFmtM
   in catMaybes [etagM, updatedAtM]
