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

data RemoteFeed = RemoteFeed URL (Maybe FeedFormat)

fromUrl :: URL -> RemoteFeed
fromUrl url = RemoteFeed url Nothing

fromFeed :: Feed -> RemoteFeed
fromFeed f = RemoteFeed (feedUrl f) (Just (feedFormat f))

-- | Import feed and entries from URL.
importEntries :: AppEnv -> RemoteFeed -> IO (FeedDetailed, [EntryDetailed])
importEntries env (RemoteFeed url (Just TwitterFeedFormat)) = Twitter.importFeedEntries env url
importEntries env (RemoteFeed url (Just MF2FeedFormat)) = Microformats2.importFeedEntries env url
importEntries env (RemoteFeed url (Just _)) = RSS.importFeedEntries env Nothing url
importEntries env (RemoteFeed url Nothing) =
  if Twitter.isProfileUrl url
    then Twitter.importFeedEntries env url
    else do
      (res, body, finalUrl) <- HTTP.fetchAndRetryUrl url
      case contentType res of
        UnsupportedContentType -> throwIO (ImportError url FormatNotSupported)
        XMLContentType -> RSS.parseFeedEntries env Nothing finalUrl body
        HTMLContentType -> do
          feedOrErr <- try $ Microformats2.parseFeedEntries env finalUrl body
          case feedOrErr of
            Left (ImportError _ _) -> do
              -- Follow any rel="alternative" XML feed URL.
              -- If no <link> found, try /feed (default RSS URL for WordPress)
              let xmlFeedUrlM = Microformats2.extractXmlFeedUrl finalUrl body <|> Just (finalUrl +> ["feed"])
              case xmlFeedUrlM of
                Nothing -> throwIO (ImportError url NoFeedFound)
                Just xmlFeedUrl -> do
                  let htmlAuthorM = Microformats2.authorFromHtml finalUrl body
                  RSS.importFeedEntries env htmlAuthorM xmlFeedUrl
            Left err -> throwIO err
            Right feed -> return feed
