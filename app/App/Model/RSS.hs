{-# LANGUAGE OverloadedStrings #-}

module App.Model.RSS where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.Model.HTTP
import qualified App.Model.Microformats2 as MF2
import App.View.Language
import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as XMLFeedImport
import qualified Text.Feed.Query as XMLFeedQuery
import qualified Text.Feed.Types as XMLFeed
import qualified Text.HTML.TagSoup as TS
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

importFeedEntries :: AppEnv -> Maybe Author -> URL -> IO (FeedDetailed, [EntryDetailed])
importFeedEntries env htmlAuthorM url = do
  (res, body, finalUrl) <- fetchAndRetryUrl url
  if contentType res == XMLContentType
    then parseFeedEntries env htmlAuthorM finalUrl body
    else throwIO (ImportError url NoFeedFound)

parseFeedEntries :: AppEnv -> Maybe Author -> URL -> BL.ByteString -> IO (FeedDetailed, [EntryDetailed])
parseFeedEntries env htmlAuthorM url xml = do
  xmlFeed <- maybe (throwIO (ImportError url NoFeedFound)) pure $ XMLFeedImport.parseFeedSource xml
  let xmlAuthor = authorFromXmlFeed url xmlFeed
      xmlFeedName = XMLFeedQuery.getFeedTitle xmlFeed
  htmlAuthor <-
    maybe
      (fromMaybe xmlAuthor . rightToMaybe <$> (try (MF2.importAuthor (authorUrl xmlAuthor)) :: IO (Either AppError Author)))
      pure
      htmlAuthorM
  let author =
        htmlAuthor
          { authorName = liftA2 min (authorName xmlAuthor) (authorName htmlAuthor),
            authorNote = authorNote htmlAuthor <|> authorNote xmlAuthor,
            authorImageUrl = authorImageUrl htmlAuthor <|> authorImageUrl xmlAuthor
          }
      feed =
        Feed
          { feedUrl = url,
            feedAuthorUrl = authorUrl author,
            feedName = Just xmlFeedName,
            feedSummary = summaryFromHtml Nothing <$> extractFeedSummary author xmlFeedName xmlFeed,
            feedFormat = feedFormatFromXml xmlFeed,
            feedRecentEntryUrl = Nothing,
            feedUpdatedAt = Nothing,
            feedImportedAt = Nothing,
            feedImportError = Nothing
          }
      entries = entriesFromXmlFeed env url feed xmlFeed
      entriesDtld = (\p -> EntryDetailed p author Nothing) <$> entries
      feedDtld = feedDetailed feed author
  FeedDetailed.timestampEntries feedDtld entriesDtld

extractFeedSummary :: Author -> Text -> XMLFeed.Feed -> Maybe Text
extractFeedSummary author feedName xmlFeed = do
  desc <- XMLFeedQuery.getFeedDescription xmlFeed
  if Just desc == authorName author && desc == feedName then Nothing else nullifyText desc

-- | Return format of XML feed.
feedFormatFromXml :: XMLFeed.Feed -> FeedFormat
feedFormatFromXml (XMLFeed.XMLFeed _) = XMLFeedFormat
feedFormatFromXml (XMLFeed.AtomFeed _) = AtomFeedFormat
feedFormatFromXml _ = RSSFeedFormat

-- | Extract an author from feed XMLFeed.
authorFromXmlFeed :: URL -> XMLFeed.Feed -> Author
authorFromXmlFeed xmlUrl feed =
  (emptyAuthor url)
    { authorName = nullifyText =<< (XMLFeedQuery.getFeedAuthor feed <|> (Just $ XMLFeedQuery.getFeedTitle feed)),
      authorImageUrl = flip relativeTo xmlUrl <$> imageUrl,
      authorNote = nullifyText =<< XMLFeedQuery.getFeedDescription feed
    }
  where
    url = fromMaybe xmlUrl (flip relativeTo xmlUrl <$> (extractFeedAuthorUrl feed))
    imageUrl = parseUrl =<< ignoreBlankImage =<< XMLFeedQuery.getFeedLogoLink feed
    ignoreBlankImage txt = if "blank.gif" `T.isInfixOf` txt then Nothing else Just txt
    extractFeedAuthorUrl (XMLFeed.AtomFeed f) = parseUrl =<< Atom.personURI =<< listToMaybe (Atom.feedAuthors f)
    extractFeedAuthorUrl (XMLFeed.RSSFeed f) = parseUrl =<< Just (RSS.rssLink (RSS.rssChannel f))
    extractFeedAuthorUrl (XMLFeed.RSS1Feed f) = parseUrl =<< Just (RSS1.channelURI (RSS1.feedChannel f))
    extractFeedAuthorUrl f = parseUrl =<< XMLFeedQuery.getFeedHome f

-- | Extract entries from RSS/Atom feed XMLFeed.
entriesFromXmlFeed :: AppEnv -> URL -> Feed -> XMLFeed.Feed -> [Entry]
entriesFromXmlFeed env baseUrl feed xmlFeed =
  let feedItems = XMLFeedQuery.feedItems xmlFeed
      entries = entryFromFeedItem env baseUrl feed <$> feedItems
   in catMaybes entries

-- | Extract entry from RSS/Atom feed item.
entryFromFeedItem :: AppEnv -> URL -> Feed -> XMLFeed.Item -> Maybe Entry
entryFromFeedItem env baseUrl feed item = do
  externalEntryUrl <- flip relativeTo baseUrl <$> (parseUrl =<< XMLFeedQuery.getItemLink item)
  let name = nullifyText =<< XMLFeedQuery.getItemTitle item
      summary = nullifyText =<< XMLFeedQuery.getItemSummary item
      content = nullifyText =<< (XMLFeedQuery.getItemContent item <|> XMLFeedQuery.getItemDescription item)
      publishedAt = parseDateTime =<< XMLFeedQuery.getItemPublishDateString item
      audioUrl = flip relativeTo baseUrl <$> (urlFromEnclosure "audio" =<< XMLFeedQuery.getItemEnclosure item)
      videoUrl = flip relativeTo baseUrl <$> (urlFromEnclosure "video" =<< XMLFeedQuery.getItemEnclosure item)
      imageUrl = flip relativeTo baseUrl <$> (urlFromEnclosure "image" =<< XMLFeedQuery.getItemEnclosure item)
      contentImageUrls = flip relativeTo baseUrl <$> fromMaybe [] (mediaFromFeedItemContent "img" <$> content)
      contentVideoUrls = flip relativeTo baseUrl <$> fromMaybe [] (mediaFromFeedItemContent "video" <$> content)
      contentAudioUrls = flip relativeTo baseUrl <$> fromMaybe [] (mediaFromFeedItemContent "audio" <$> content)
      entry = emptyEntry externalEntryUrl (feedAuthorUrl feed) (feedUrl feed)
  return $
    entry
      { entryName = if name == content then Nothing else name,
        entrySummary = summaryFromHtml (Just summaryLength) <$> (nullifyText =<< (summary <|> content)),
        entryContent = sanitizeAndProxyImageHtml env baseUrl . wrapInParagraph <$> content,
        entryImageUrls = L.nub (maybeToList imageUrl <> contentImageUrls),
        entryAudioUrls = L.nub (maybeToList audioUrl <> contentAudioUrls),
        entryVideoUrls = L.nub (maybeToList videoUrl <> contentVideoUrls),
        entryPublishedAt = publishedAt
      }

mediaFromFeedItemContent :: Text -> Text -> [URL]
mediaFromFeedItemContent mediaTagName content =
  let extractSrcUrl ("src", val) = parseUrl val
      extractSrcUrl _ = Nothing
      extractSrcUrls attrs = listToMaybe (catMaybes (extractSrcUrl <$> attrs))
      extractMedia b (TS.TagOpen name attrs : ts) =
        if name == mediaTagName then extractMedia (maybe b (: b) (extractSrcUrls attrs)) ts else b
      extractMedia b _ = b
   in extractMedia [] (TS.parseTags content)

type FormatPrefix = Text

-- | Extract URL from feed enclosure if it matches the given mime-type prefix (audio, video, etc.).
urlFromEnclosure :: FormatPrefix -> (Text, Maybe Text, Maybe Integer) -> Maybe URL
urlFromEnclosure formatPrefix (uri, format, _) =
  if T.take 5 (fromMaybe "" format) == formatPrefix then parseUrl uri else Nothing
