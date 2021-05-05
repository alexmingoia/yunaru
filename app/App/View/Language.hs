module App.View.Language where

import App.Model.EmailAddress
import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed
import App.Model.Image
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Numeric as Numeric
import qualified Text.HTML.SanitizeXSS as XSS
import qualified Text.HTML.TagSoup as TS

summaryLength :: Int
summaryLength = 280

-- | Render plaintext summary of HTML, truncated to optional maximum length.
summaryFromHtml :: Maybe Int -> Text -> Text
summaryFromHtml max html =
  let plaintext = T.unwords (catMaybes (fmap T.strip . TS.maybeTagText <$> TS.parseTags html))
   in maybe plaintext (flip truncateOnWord plaintext) max

-- Return <h1> text from HTML, and HTML after <h1> has been extracted.
splitTitleAndContent :: Text -> (Maybe Text, Maybe Text)
splitTitleAndContent html =
  let (title, content) = fst (L.foldl' go (("", []), False) (TS.parseTags html))
   in (nullifyText title, nullifyText (TS.renderTags (L.reverse content)))
  where
    go ((a, b), True) (TS.TagText txt) = ((a <> txt, b), True)
    go ((a, b), True) (TS.TagClose "h1") = ((a, b), False)
    go ((a, b), True) _ = ((a, b), True)
    go ((a, b), False) (TS.TagOpen "h1" _) = ((a, b), True)
    go ((a, b), False) tag = ((a, tag : b), False)

-- | Truncate on word.
truncateOnWord max txt =
  let stripped = T.strip txt
      ws = T.words (T.take max stripped)
   in if T.length stripped > max
        then T.unwords (if L.length ws < 2 then ws else L.init ws)
        else stripped

-- | Truncate HTML. Truncates text on word and adds ellipsis.
truncateHTML :: Int -> Text -> Text
truncateHTML max = TS.renderTags . go 0 0 . TS.parseTags
  where
    go _ _ [] = []
    go depth len (t@(TS.TagOpen _ _) : ts) =
      if len >= max then go (depth + 1) len ts else t : (go depth len ts)
    go depth len (t@(TS.TagText text) : ts) =
      if (len + T.length text) > max
        then
          if len >= max
            then go depth max ts
            else TS.TagText (truncateOnWord (max - len) text <> "...") : (go depth max ts)
        else t : (go depth (len + T.length text) ts)
    go 0 len (t@(TS.TagClose _) : ts) =
      t : go 0 len ts
    go depth len ((TS.TagClose _) : ts) =
      go (depth - 1) len ts
    go depth len (_ : ts) = go depth len ts

pluralizeCount :: Int -> Text -> Text
pluralizeCount n w = if n == 1 then w else w <> "s"

possessive :: Text -> Text
possessive w = if "s" `T.isSuffixOf` w then w <> "'" else w <> "'s"

formatNumberAbbrev :: RealFloat a => a -> String
formatNumberAbbrev n
  | n < 1000 = Numeric.showFFloat (Just 0) n ""
  | n >= 1000 = Numeric.showFFloat (Just 1) (n / 1000) "k"
  | n > 1000000 = Numeric.showFFloat (Just 1) (n / 1000000) "m"
  | otherwise = Numeric.showFFloat (Just 1) (n / 1000000000) "b"

rsvpHuman :: Text -> Text
rsvpHuman "yes" = "I will be attending"
rsvpHuman "no" = "I will not be attending"
rsvpHuman "maybe" = "I might attend"
rsvpHuman "interested" = "I am interested in attending"
rsvpHuman _ = "I am interested in attending"

entryPageTitle entryDtld =
  let e = entryInfo entryDtld
      a = entryAuthor entryDtld
      authorName' = fromMaybe (renderDisplayUrl (authorUrl a)) (authorName a)
      entrySummary' = maybe defaultSummary ((authorName' <> ": ") <>) (entrySummary e)
      defaultSummary = authorName' <> " posted"
   in fromMaybe entrySummary' (entryName e)

feedPageTitle feedDtld =
  let feed = feedInfo feedDtld
      author = feedAuthor feedDtld
   in fromMaybe (renderDisplayUrl (feedUrl feed)) (feedName feed <|> authorName author)

feedPageDesc env feedDtld =
  let f = feedInfo feedDtld
      a = feedAuthor feedDtld
      defaultLocalSummary = "Latest blog posts by " <> feedDisplayName feedDtld <> "."
      defaultExternalSummary = "Follow " <> feedDisplayName feedDtld <> " using " <> appName env <> "."
      defaultSummary = if isJust (feedImportedAt f) then defaultExternalSummary else defaultLocalSummary
      feedSummary' = summaryFromHtml Nothing <$> (nullifyText =<< (feedSummary f <|> authorNote a))
   in if isJust (feedImportedAt f)
        then defaultSummary
        else fromMaybe defaultSummary feedSummary'

formatTime8601 :: UTCTime -> Text
formatTime8601 = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

formatTime822 :: UTCTime -> Text
formatTime822 = pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S %Z"

formatTimeHuman :: UTCTime -> Text
formatTimeHuman = pack . formatTime defaultTimeLocale "%h %e, %Y"

type Now = UTCTime

formatTimeAgo :: Now -> UTCTime -> Text
formatTimeAgo now ts
  | diffUTCTime now ts >= 1209600 = formatTimeHuman ts
  | diffUTCTime now ts >= 604800 = pack (show (floor (diffUTCTime now ts) `quot` 604800)) <> "w"
  | diffUTCTime now ts >= 86400 = pack (show (floor (diffUTCTime now ts) `quot` 86400)) <> "d"
  | diffUTCTime now ts >= 3600 = pack (show (floor (diffUTCTime now ts) `quot` 3600)) <> "h"
  | diffUTCTime now ts >= 120 = pack (show (floor (diffUTCTime now ts) `quot` 60)) <> "m"
  | otherwise = "now"

formatTimeAgoCompact :: Now -> UTCTime -> Text
formatTimeAgoCompact now ts
  | diffUTCTime now ts >= 29030400 = pack (show (floor (diffUTCTime now ts) `quot` 29030400)) <> "yr"
  | diffUTCTime now ts >= 2419200 = pack (show (floor (diffUTCTime now ts) `quot` 2419200)) <> "mo"
  | diffUTCTime now ts >= 604800 = pack (show (floor (diffUTCTime now ts) `quot` 604800)) <> "w"
  | diffUTCTime now ts >= 86400 = pack (show (floor (diffUTCTime now ts) `quot` 86400)) <> "d"
  | diffUTCTime now ts >= 3600 = pack (show (floor (diffUTCTime now ts) `quot` 3600)) <> "h"
  | diffUTCTime now ts >= 120 = pack (show (floor (diffUTCTime now ts) `quot` 60)) <> "m"
  | otherwise = "now"

nullifyText :: Text -> Maybe Text
nullifyText txt =
  let stripped = T.strip txt
   in if T.null stripped then Nothing else Just stripped

-- | Date/Time
parseDateTime :: Text -> Maybe UTCTime
parseDateTime ds =
  let formats =
        [ "%Y-%m-%d",
          "%Y-%m-%dT%H:%M:%S",
          "%Y-%m-%dT%H:%M:%SZ",
          "%Y-%m-%dT%H:%M:%S%QZ",
          "%Y-%m-%dT%H:%M:%S%z",
          "%Y-%m-%dT%H:%M:%S%Q%z",
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%d %H:%M:%SZ",
          "%Y-%m-%d %H:%M:%S%QZ",
          "%Y-%m-%d %H:%M:%S%z",
          "%Y-%m-%d %H:%M:%S%Q%z",
          "%Y-%m-%d %H:%M:%S %z",
          "%Y-%m-%d %H:%M:%S%Q %z",
          "%Y-%m-%d %H:%M:%S %Z",
          "%Y-%m-%d %H:%M:%S%Q %Z",
          "%Y-%m-%d %H:%M:%S %z %Z",
          "%Y-%m-%d %H:%M:%S%Q %z %Z",
          "%a, %_d %b %Y %H:%M:%S %Z",
          "%d %b %Y",
          "%b %d %Y"
        ]
   in L.foldl1 mplus (fmap (\fmt -> parseTimeM True defaultTimeLocale fmt $ T.unpack ds) formats)

-- | Wrap text in paragraph tag if it's not wrapped in a tag.
wrapInParagraph :: Text -> Text
wrapInParagraph txt = if "<p" `T.isPrefixOf` txt || "<h" `T.isPrefixOf` txt then txt else "<p>" <> txt <> "</p>"

-- | Strip all HTML except safe anchor tags.
onlyAnchorHtml :: Text -> Text
onlyAnchorHtml html = TS.renderTags (catMaybes (onlyAnchor <$> TS.parseTags html))
  where
    onlyAnchor (TS.TagOpen "a" attrs) = Just (TS.TagOpen "a" (catMaybes (XSS.sanitizeAttribute <$> attrs)))
    onlyAnchor tag@(TS.TagClose "a") = Just tag
    onlyAnchor tag@(TS.TagText _) = Just tag
    onlyAnchor _ = Nothing

-- | Strip unsafe HTML and balance tags.
sanitizeHtml :: Text -> Text
sanitizeHtml = XSS.filterTags (wrapImageTags . sanitizeTags) . replaceNewlines

replaceNewlines :: Text -> Text
replaceNewlines = T.replace "\n" "<br />" . T.replace "\r\n" "<br />"

sanitizeAndProxyImageHtml :: AppEnv -> URL -> Text -> Text
sanitizeAndProxyImageHtml env baseUrl = XSS.filterTags (wrapImageTags . proxyImageTags env baseUrl . sanitizeTags)

sanitizeTags :: [TS.Tag Text] -> [TS.Tag Text]
sanitizeTags =
  L.filter (not . disallowedTag) . fmap stripClass . XSS.balanceTags . XSS.safeTags
  where
    disallowedTag (TS.TagOpen n _) = n `S.member` disallowedTags
    disallowedTag (TS.TagClose n) = n `S.member` disallowedTags
    disallowedTag _ = False
    stripClass (TS.TagOpen n attrs) = TS.TagOpen n (L.filter ((/= "class") . fst) attrs)
    stripClass t = t

wrapImageTags :: [TS.Tag Text] -> [TS.Tag Text]
wrapImageTags (t1@(TS.TagOpen "figure" _) : t2@(TS.TagOpen "img" _) : ts) =
  t1 : t2 : wrapImageTags ts
wrapImageTags (t1@(TS.TagOpen "img" _) : t2@(TS.TagClose "img") : ts) =
  TS.TagOpen "figure" [] : t1 : t2 : TS.TagClose "figure" : wrapImageTags ts
wrapImageTags (t : ts) = t : wrapImageTags ts
wrapImageTags [] = []

proxyImageTags :: AppEnv -> URL -> [TS.Tag Text] -> [TS.Tag Text]
proxyImageTags env baseUrl = fmap replaceImage
  where
    replaceImage (TS.TagOpen "img" attrs) = TS.TagOpen "img" (replaceUrl <$> attrs)
    replaceImage tag = tag
    replaceUrl ("src", value) = ("src", maybe value (renderUrl . cacheUrl env 800 800 False . flip relativeTo baseUrl) (parseUrl value))
    replaceUrl attr = attr

disallowedTags :: S.Set Text
disallowedTags =
  S.fromList
    [ "article",
      "header",
      "footer",
      "input",
      "textarea",
      "option",
      "select",
      "section",
      "command",
      "optgroup",
      "var",
      "nav",
      "button"
    ]

-- | Map newlines to <br /> tags.
breakify :: Text -> Text
breakify txt = T.foldl' (\b a -> if a == '\n' then b <> "<br />" else T.snoc b a) "" txt

unbreakify :: Text -> Text
unbreakify html = T.replace "<br />" "\n" html

-- | Map URLs to <a> tags.
linkify :: Text -> Text
linkify txt = T.concat (wordToAnchor <$> T.groupBy groupSpace txt)
  where
    anchorUrlHtml url = "<a href=\"" <> renderUrl url <> "\">" <> renderDisplayUrl url <> "</a>"
    anchorEmailHtml email = "<a href=\"mailto:" <> email <> "\">" <> email <> "</a>"
    wordToAnchor w =
      if hasPeriod w
        then
          if isJust (parseEmail w)
            then anchorEmailHtml w
            else maybe w anchorUrlHtml (parseInputUrl w)
        else w
    hasPeriod w = T.take 1 w /= "." && T.takeEnd 1 w /= "." && T.any ('.' ==) w
    groupSpace a b = isSpace a == isSpace b

unlinkify :: Text -> Text
unlinkify html = TS.renderTags (textifyAnchors (TS.parseTags html))
  where
    href = fmap snd . L.find ((== "href") . fst)
    emailHrefMatchesLabel href label =
      if "mailto:" `T.isPrefixOf` href && T.drop 7 href == label then Just label else Nothing
    urlHrefMatchesLabel href label =
      if href == label || maybe "" renderDisplayUrl (parseUrl href) == label then Just href else Nothing
    unlink label href =
      urlHrefMatchesLabel href label <|> emailHrefMatchesLabel href label
    textifyAnchors (t1@(TS.TagOpen "a" attrs) : t2@(TS.TagText txt) : t3@(TS.TagClose "a") : ts) =
      case unlink txt =<< href attrs of
        Nothing -> t1 : t2 : t3 : textifyAnchors ts
        Just href -> TS.TagText href : textifyAnchors ts
    textifyAnchors (t : ts) = t : textifyAnchors ts
    textifyAnchors ([]) = []
