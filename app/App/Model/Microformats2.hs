{-# LANGUAGE OverloadedStrings #-}

module App.Model.Microformats2 where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.Model.HTTP
import App.View.Language
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Microformats2.Parser
import Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import qualified Data.Vector as V
import Network.URI (parseURI)
import Text.HTML.Scalpel.Core
import Text.Read (readMaybe)

importFeedEntries :: AppEnv -> URL -> IO (FeedDetailed, [EntryDetailed])
importFeedEntries env url = do
  (res, body, finalUrl) <- fetchAndRetryUrl url
  if contentType res == HTMLContentType
    then parseFeedEntries env finalUrl body
    else throwIO (ImportError url NoFeedFound)

parseFeedEntries :: AppEnv -> URL -> BL.ByteString -> IO (FeedDetailed, [EntryDetailed])
parseFeedEntries env url html = do
  let opts = def {baseUri = parseURI . T.unpack $ renderUrl url}
      mf2 = parseMf2 opts $ documentRoot $ parseLBS html
      (hcards, hentries) = L.partition (hasMf2Type "h-card") (extractMf2Types ["h-card", "h-entry"] mf2)
      rels = extractRels mf2
      authorM =
        (fmap mergeAuthors <$> nonEmpty) $
          catMaybes
            [ representativeAuthorFromMf2Json url rels hcards,
              authorFromHtml url html
            ]
  author <- maybe (throwIO (ImportError url NoFeedFound)) pure authorM
  let feed = feedFromMf2Json url author mf2
      entriesDtld = entriesFromMf2Json env url feed author hentries
  if L.null entriesDtld
    then throwIO (ImportError url NoFeedFound)
    else FeedDetailed.timestampEntries (feedDetailed feed author) entriesDtld

-- | Extract RSS/Atom feed URL from HTML.
extractXmlFeedUrl :: URL -> BL.ByteString -> Maybe URL
extractXmlFeedUrl baseUrl html = do
  href <-
    scrapeStringLike html $
      (attr "href" $ "link" @: ["rel" @= "alternate", "type" @= "application/rss+xml"])
        <|> (attr "href" $ "link" @: ["rel" @= "alternate", "type" @= "application/atom+xml"])
        <|> (attr "href" $ "link" @: ["rel" @= "alternative", "type" @= "application/rss+xml"])
        <|> (attr "href" $ "link" @: ["rel" @= "alternative", "type" @= "application/atom+xml"])
  url <- parseUrl $ lbsToTxt href
  return $ url `relativeTo` baseUrl

importAuthor :: URL -> IO Author
importAuthor url = do
  (res, body, finalUrl) <- fetchAndRetryUrl url
  if contentType res /= HTMLContentType
    then throwIO (ImportError url NoAuthorFound)
    else do
      let opts = def {baseUri = parseURI . T.unpack $ renderUrl finalUrl}
          mf2 = parseMf2 opts $ documentRoot $ parseLBS body
          (hcards, _) = L.partition (hasMf2Type "h-card") (extractMf2Types ["h-card", "h-entry"] mf2)
          rels = extractRels mf2
          authorM =
            (fmap mergeAuthors <$> nonEmpty) $
              catMaybes
                [ representativeAuthorFromMf2Json url rels hcards,
                  firstAuthorMatchingHostFromMf2Json url hcards,
                  authorFromHtml finalUrl body
                ]
      maybe (throwIO (ImportError finalUrl NoAuthorFound)) pure authorM

-- | Extract an h-card from page title and meta tags.
authorFromHtml :: URL -> BL.ByteString -> Maybe Author
authorFromHtml url html = scrapeStringLike html $ do
  name <- text "title"
  desc <- listToMaybe <$> (attrs "content" $ "meta" @: ["name" @= "description"])
  ogImageUrl <- listToMaybe <$> (attrs "content" $ "meta" @: ["property" @= "og:image"])
  iconUrl <-
    listToMaybe
      <$> chroots
        "link"
        ( do
            (attr "href" $ "link" @: ["rel" @= "icon", "sizes" @= "128x128"])
              <|> (attr "href" $ "link" @: ["rel" @= "shortcut icon", "sizes" @= "64x64"])
              <|> (attr "href" $ "link" @: ["rel" @= "icon", "sizes" @= "64x64"])
              <|> (attr "href" $ "link" @: ["rel" @= "apple-touch-icon"])
              <|> (attr "href" $ "link" @: ["rel" @= "shortcut icon", "sizes" @= "48x48"])
              <|> (attr "href" $ "link" @: ["rel" @= "icon", "sizes" @= "48x48"])
              <|> (attr "href" $ "link" @: ["rel" @= "shortcut icon", "sizes" @= "32x32"])
              <|> (attr "href" $ "link" @: ["rel" @= "icon", "sizes" @= "32x32"])
              <|> (attr "href" $ "link" @: ["rel" @= "icon"])
              <|> (attr "href" $ "link" @: ["rel" @= "shortcut icon"])
        )
  let imageUrl = iconUrl <|> ogImageUrl
  return $
    (emptyAuthor url)
      { authorName = nullifyText $ lbsToTxt name,
        authorImageUrl = flip relativeTo url <$> (parseUrl =<< lbsToTxt <$> imageUrl),
        authorNote = nullifyText =<< (lbsToTxt <$> desc)
      }

-- | Find the representative author for given URL and its MF2 JSON.
--
-- Implements the representative h-card algorithm
-- http://microformats.org/wiki/representative-h-card-parsing
representativeAuthorFromMf2Json :: URL -> Rels -> [JSON.Value] -> Maybe Author
representativeAuthorFromMf2Json baseUrl rels hcards =
  let relMes = fromMaybe [] (HM.lookup "me" rels)
      hcard = L.find matchUrlAndUid hcards <|> L.find hasRelMe hcards <|> L.find matchUrl hcards
      hasRelMe = maybe False (`L.elem` relMes) . getMf2UrlProp "url"
      matchUrl = maybe False ((withoutScheme baseUrl ==) . withoutScheme) . getMf2UrlProp "url"
      matchUid = maybe False ((withoutScheme baseUrl ==) . withoutScheme) . getMf2UrlProp "uid"
      matchUrlAndUid json = matchUrl json && matchUid json
   in authorFromMf2Json =<< hcard

firstAuthorMatchingHostFromMf2Json :: URL -> [JSON.Value] -> Maybe Author
firstAuthorMatchingHostFromMf2Json baseUrl hcards =
  let hcard = L.find matchUrlAuthority hcards
      matchUrlAuthority = maybe False ((urlAuthority baseUrl ==) . urlAuthority) . getMf2UrlProp "url"
   in authorFromMf2Json =<< hcard

-- | Decode h-card from MF2 JSON object.
authorFromMf2Json :: JSON.Value -> Maybe Author
authorFromMf2Json json =
  case getMf2UrlProp "url" json of
    Just url ->
      Just $
        (emptyAuthor url)
          { authorName = nullifyText =<< getMf2TextProp "name" json,
            authorImageUrl = getMf2UrlProp "photo" json <|> getMf2UrlProp "logo" json,
            authorNote = nullifyText =<< getMf2TextProp "note" json
          }
    Nothing -> Nothing

feedFromMf2Json :: URL -> Author -> JSON.Value -> Feed
feedFromMf2Json url author json =
  let hfeed = listToMaybe (extractMf2Types ["h-feed"] json)
   in Feed
        { feedUrl = url,
          feedAuthorUrl = authorUrl author,
          feedName = nullifyText =<< getMf2TextProp "name" =<< hfeed,
          feedSummary = summaryFromHtml Nothing <$> (nullifyText =<< getMf2TextProp "summary" =<< hfeed),
          feedFormat = MF2FeedFormat,
          feedRecentEntryUrl = Nothing,
          feedUpdatedAt = Nothing,
          feedImportedAt = Nothing,
          feedImportError = Nothing,
          feedCreatedAt = Nothing
        }

-- | Extract entries from JSON encoded MF2 h-entries.
entriesFromMf2Json :: AppEnv -> URL -> Feed -> Author -> [JSON.Value] -> [EntryDetailed]
entriesFromMf2Json env baseUrl feed author hentries =
  let entries = entryOrReblogFromMf2Json env baseUrl feed author <$> hentries
   in catMaybes entries

entryOrReblogFromMf2Json :: AppEnv -> URL -> Feed -> Author -> JSON.Value -> Maybe EntryDetailed
entryOrReblogFromMf2Json env baseUrl feed author json =
  case getMf2Prop "repost-of" json of
    Nothing -> do
      entryDtld <- entryFromMf2Json env baseUrl feed author json
      if isJust (entryPublishedAt (entryInfo entryDtld)) then return entryDtld else Nothing
    Just reblogOfJson -> do
      recentPublishedAt <- getMf2RecentDate "published" json
      reblogEntryDtld <- entryFromMf2Json env baseUrl feed author reblogOfJson
      let reblogAuthor = fromMaybe author (getMf2AuthorProp json)
          rebloggedAt = Just recentPublishedAt
          reblogEntry = entryInfo reblogEntryDtld
          reblogPublishedAt = getMf2TextProp "published" json >>= parseDateTime
      return $
        reblogEntryDtld
          { entryInfo =
              reblogEntry
                { entryRebloggedAt = rebloggedAt,
                  entryRebloggedBy = Just (authorUrl reblogAuthor),
                  entryPublishedAt = entryPublishedAt reblogEntry <|> reblogPublishedAt
                },
            entryReblogAuthor = Just reblogAuthor
          }

-- | Decode entry from MF2 JSON h-entry.
entryFromMf2Json :: AppEnv -> URL -> Feed -> Author -> JSON.Value -> Maybe EntryDetailed
entryFromMf2Json env baseUrl feed author json@(JSON.Object _) = do
  url <- getMf2UrlProp "url" json
  let featuredAuthor = fromMaybe author (getMf2AuthorProp json)
      featured = getMf2UrlProps "featured" json
      photos = getMf2UrlProps "photo" json
      eventSummaryM = do
        rsvp <- rsvpHuman <$> getMf2TextProp "rsvp" json
        eventJson <- listToMaybe (extractMf2Types ["h-event"] json)
        eventName <- getMf2TextProp "name" eventJson
        eventUrl <- getMf2TextProp "url" eventJson
        eventTime <- parseDateTime =<< getMf2TextProp "start" eventJson
        return $ rsvp <> " " <> "<a href=\"" <> eventUrl <> "\">" <> eventName <> "</a> on " <> formatTimeHuman eventTime
      summary = getMf2TextProp "summary" json <|> eventSummaryM
      content = fmap fst . getMf2ContentProp =<< getMf2Prop "content" json
      entry = emptyEntry url (authorUrl featuredAuthor) (feedUrl feed)
  return $
    EntryDetailed
      { entryInfo =
          entry
            { entryName = getHEntryName json,
              entrySummary = summaryFromHtml (Just summaryLength) <$> (nullifyText =<< (summary <|> content)),
              entryContent = wrapInParagraph <$> (nullifyText =<< (sanitizeAndProxyImageHtml env baseUrl <$> content)),
              entryImageUrls = featured <> photos,
              entryAudioUrls = getMf2UrlProps "audio" json,
              entryVideoUrls = getMf2UrlProps "video" json,
              entryPublishedAt = getMf2TextProp "published" json >>= parseDateTime,
              entryUpdatedAt = getMf2TextProp "updated" json >>= parseDateTime
            },
        entryAuthor = featuredAuthor,
        entryReblogAuthor = Nothing
      }
entryFromMf2Json _ _ _ _ _ = Nothing

mergeAuthors :: NonEmpty Author -> Author
mergeAuthors as = L.foldl' merge (NE.head as) as
  where
    merge b a =
      b
        { authorName = authorName b <|> authorName a,
          authorImageUrl = authorImageUrl b <|> authorImageUrl a,
          authorNote = authorNote b <|> authorNote a
        }

type Rel = Text

type Rels = HM.HashMap Rel [URL]

extractRels :: JSON.Value -> Rels
extractRels (JSON.Object rm) =
  case HM.lookup "rels" rm of
    Just (JSON.Object rsm) -> (catMaybes . fmap parseUrl . jsonTextList) <$> rsm
    _ -> HM.empty
extractRels _ = HM.empty

jsonTextList :: JSON.Value -> [Text]
jsonTextList (JSON.Array arr) = catMaybes $ jsonText <$> V.toList arr
jsonTextList _ = []

jsonText :: JSON.Value -> Maybe Text
jsonText (JSON.String txt) = Just txt
jsonText _ = Nothing

-- | Traverse MF2 JSON and return list of all MF2 objects of given types.
extractMf2Types :: [Text] -> JSON.Value -> [JSON.Value]
extractMf2Types types = go []
  where
    go vs obj@(JSON.Object hm) =
      if L.any (flip hasMf2Type obj) types
        then obj : vs <> HM.foldl' go [] hm
        else vs <> HM.foldl' go [] hm
    go vs (JSON.Array arr) = vs <> V.foldl' go [] arr
    go vs _ = vs

-- | Check if JSON value is an object with given MF2 type.
hasMf2Type :: Text -> JSON.Value -> Bool
hasMf2Type typ (JSON.Object hm) =
  case HM.lookup "type" hm of
    Just (JSON.Array arr) -> JSON.String typ `elem` arr
    _ -> False
hasMf2Type _ _ = False

-- | Get h-entry name value, if the e-content value does not match.
--
-- This is to avoid importing entries with duplicate names and content,
-- which are a pattern often seen when parsing "note" entries.
getHEntryName :: JSON.Value -> Maybe Text
getHEntryName entry =
  let name = nullifyText =<< getMf2TextProp "name" entry
      contentPlaintext = nullifyText =<< fmap snd . getMf2ContentProp =<< getMf2Prop "content" entry
   in if contentPlaintext == name then Nothing else name

type HTML = Text

type Plaintext = Text

-- | Get the MF2 e-content property, which has both HTML and plaintext values.
getMf2ContentProp :: JSON.Value -> Maybe (HTML, Plaintext)
getMf2ContentProp (JSON.String val) = Just (val, val)
getMf2ContentProp (JSON.Object hm) =
  let content = do
        html <- HM.lookup "html" hm
        plaintext <- HM.lookup "value" hm
        return (html, plaintext)
   in case content of
        Just (JSON.String html, JSON.String plaintext) -> Just (html, plaintext)
        _ -> Nothing
getMf2ContentProp _ = Nothing

getText :: JSON.Value -> Maybe Text
getText (JSON.String txt) = Just txt
getText (JSON.Object hm) =
  case HM.lookup "value" hm of
    Just (JSON.String txt) -> Just txt
    _ -> Nothing
getText _ = Nothing

getMf2TextProp :: Text -> JSON.Value -> Maybe Text
getMf2TextProp key json = getText =<< getMf2Prop key json

getMf2TextProps :: Text -> JSON.Value -> [Text]
getMf2TextProps key json = catMaybes (getText <$> getMf2Props key json)

getMf2IntProp :: Text -> JSON.Value -> Maybe Int
getMf2IntProp key json = readMaybe =<< (T.unpack <$> getMf2TextProp key json)

getMf2UrlProp :: Text -> JSON.Value -> Maybe URL
getMf2UrlProp key json = parseUrl =<< getMf2TextProp key json

getMf2UrlProps :: Text -> JSON.Value -> [URL]
getMf2UrlProps key json = catMaybes ((parseUrl <=< getText) <$> getMf2Props key json)

getMf2RecentDate :: Text -> JSON.Value -> Maybe UTCTime
getMf2RecentDate key json =
  L.foldl' greatestDate Nothing (catMaybes (parseDateTime <$> getMf2TextProps key json))
  where
    greatestDate b a = if fromMaybe a b >= a then Just a else b

-- | Get array values from MF2 object property.
getMf2Props :: Text -> JSON.Value -> [JSON.Value]
getMf2Props key (JSON.Object hm) =
  case HM.lookup "properties" hm of
    Just (JSON.Object phm) ->
      case HM.lookup key phm of
        Just (JSON.Array arr) -> V.toList arr
        _ -> []
    _ -> []
getMf2Props _ _ = []

-- | Get first array value from MF2 object property.
getMf2Prop :: Text -> JSON.Value -> Maybe JSON.Value
getMf2Prop key val = listToMaybe (getMf2Props key val)

-- | Get h-card from MF2 JSON object "author" property.
getMf2AuthorProp :: JSON.Value -> Maybe Author
getMf2AuthorProp (JSON.Object hm) =
  case HM.lookup "properties" hm of
    Just (JSON.Object phm) ->
      case HM.lookup "author" phm of
        Just (JSON.Array arr) -> authorFromMf2Json $ V.head arr
        _ -> Nothing
    _ -> Nothing
getMf2AuthorProp _ = Nothing

lbsToTxt :: BL.ByteString -> Text
lbsToTxt = decodeUtf8 . B.concat . BL.toChunks
