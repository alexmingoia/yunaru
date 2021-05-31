{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.Twitter where

import App.Model.EntryDetailed
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.View.Language
import Control.Exception
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators
import Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Text.Blaze.Html ((!), preEscapedToHtml, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)
import Text.URI
import qualified Web.Twitter.Conduit as TW
import Web.Twitter.Types as TWT

importFeedEntries :: AppEnv -> URL -> IO (FeedDetailed, [EntryDetailed])
importFeedEntries env url = do
  username <- either throwIO pure (twitterUsername url)
  let consumerKeys = (encodeUtf8 (appTwConsumerKey env), encodeUtf8 (appTwConsumerSecret env))
  let tokens =
        TW.twitterOAuth
          { TW.oauthConsumerKey = fst consumerKeys,
            TW.oauthConsumerSecret = snd consumerKeys
          }
      credential = TW.Credential []
      twInfo = TW.setCredential tokens credential TW.def
  mgr <- TW.newManager TW.tlsManagerSettings
  user <- TW.call twInfo mgr (TW.usersShow username)
  timeline <-
    TW.call
      twInfo
      mgr
      ( TW.statusesUserTimeline username
          & #count
          ?~ 50
          & #contributor_details
          ?~ False
          & #tweet_mode
          ?~ ("extended" :: Text)
          & #exclude_replies
          ?~ True
          & #include_rts
          ?~ True
          & #trim_user
          ?~ False
      )
  let author = authorFromTwUser user -- TODO: linkify bio/note
      feed =
        Feed
          { feedUrl = authorUrl author,
            feedAuthorUrl = authorUrl author,
            feedName = Nothing,
            feedSummary = Nothing,
            feedFormat = TwitterFeedFormat,
            feedRecentEntryUrl = Nothing,
            feedUpdatedAt = Nothing,
            feedImportedAt = Nothing,
            feedImportError = Nothing,
            feedCreatedAt = Nothing
          }
      entriesDtld = catMaybes (entryFromTwStatus feed author <$> timeline)
  FeedDetailed.timestampEntries (feedDetailed feed author) entriesDtld

authorFromTwUser :: TWT.User -> Author
authorFromTwUser u =
  (emptyAuthor (userUrl u))
    { authorName = Just $ userName u,
      authorNote = nullifyText =<< userDescription u,
      authorImageUrl = parseAbsoluteUrl =<< userProfileImageURLHttps u
    }

entryFromTwStatus :: Feed -> Author -> Status -> Maybe EntryDetailed
entryFromTwStatus feed author s =
  let featuredStatus = fromMaybe s (statusRetweetedStatus s)
      quotedStatusM = statusQuotedStatus s
      quotedStatusHtml = fromMaybe "" (tweetBlockquoteHtml <$> quotedStatusM)
      isReply = isJust (statusInReplyToStatusId featuredStatus)
      isRetweet = isJust (statusRetweetedStatus s)
      featuredStatusId = pack (show (statusId featuredStatus))
      featuredAuthor = fromMaybe author (authorFromTwUser . statusUser <$> statusRetweetedStatus s)
      originalAuthor = author
      media = fromMaybe [] ((fmap entityBody) . exeMedia <$> statusExtendedEntities (fromMaybe featuredStatus quotedStatusM))
      imageUrls = catMaybes (parseUrl . TWT.exeMediaUrlHttps <$> (L.filter (matchTwImageType . TWT.exeType) media))
      videoUrls = catMaybes (parseUrl . TWT.exeMediaUrlHttps <$> (L.filter ((== "video") . TWT.exeType) media))
      summary = withoutRetweetPrefix (withUrls featuredStatus)
      contentM = flip append quotedStatusHtml . wrapInParagraph <$> (nullifyText (withBreakTags summary))
      url = authorUrl featuredAuthor +> ["status", featuredStatusId]
      entry = emptyEntry url (authorUrl featuredAuthor) (feedUrl feed)
   in if not isReply
        then
          Just $
            EntryDetailed
              { entryInfo =
                  entry
                    { entrySummary = nullifyText =<< Just summary,
                      entryContent = contentM,
                      entryImageUrls = imageUrls,
                      entryVideoUrls = videoUrls,
                      entryRebloggedBy = if isRetweet then Just (authorUrl originalAuthor) else Nothing,
                      entryRebloggedAt = if isRetweet then Just (statusCreatedAt s) else Nothing,
                      entryPublishedAt = Just (statusCreatedAt featuredStatus)
                    },
                entryAuthor = featuredAuthor,
                entryReblogAuthor = if isRetweet then Just originalAuthor else Nothing
              }
        else Nothing

matchTwImageType :: Text -> Bool
matchTwImageType "photo" = True
matchTwImageType "animated_gif" = True
matchTwImageType _ = False

screenNameUrl :: Text -> URL
screenNameUrl sn = fromJust $ parseUrl ("https://twitter.com/" <> sn)

userUrl :: TWT.User -> URL
userUrl = screenNameUrl . userScreenName

twitterUsername :: URL -> Either AppError TW.UserParam
twitterUsername url@(URL (URI _ (Right auth) (Just (_, path)) _ _)) =
  let host = unRText $ authHost auth
      username = unRText $ NE.head path
   in if NE.length path == 1 && (host == "twitter.com" || host == "www.twitter.com" || host == "mobile.twitter.com")
        then Right (TW.ScreenNameParam (unpack username))
        else Left (ImportError url FormatNotSupported)
twitterUsername url = Left (ImportError url FormatNotSupported)

twitterStatusId :: URL -> Either AppError TWT.StatusId
twitterStatusId url@(URL (URI _ (Right auth) (Just (_, path)) _ _)) =
  let host = unRText $ authHost auth
      pieces = unRText <$> NE.toList path
      statusIdM =
        if NE.length path == 3 && (host == "twitter.com" || host == "www.twitter.com" || host == "mobile.twitter.com")
          then case pieces of
            [_, "status", id] -> readMaybe (unpack id)
            _ -> Nothing
          else Nothing
   in maybe (Left (ImportError url FormatNotSupported)) Right statusIdM
twitterStatusId url = Left (ImportError url FormatNotSupported)

isProfileUrl :: URL -> Bool
isProfileUrl url = isRight (twitterUsername url) || isRight (twitterStatusId url)

isTwitterProfileUrl :: URL -> Bool
isTwitterProfileUrl = isRight . twitterUsername

isTweetUrl :: URL -> Bool
isTweetUrl = isRight . twitterStatusId

tweetBlockquoteHtml :: Status -> Text
tweetBlockquoteHtml s = decodeUtf8 $ BL.toStrict $ renderHtml $ do
  H.blockquote $ do
    tweetBlockquoteAuthorHtml s
    tweetBlockquoteContentHtml s

tweetBlockquoteContentHtml s =
  preEscapedToHtml $ withBreakTags $ withoutRetweetPrefix (withUrls s)

tweetBlockquoteAuthorHtml s = do
  let u = statusUser s
      id = statusId s
      name = userName u
      url =
        fromJust $ parseUrl $
          "https://twitter.com/"
            <> userScreenName u
            <> "/status/"
            <> T.pack (show id)
  H.p $ do
    H.a ! A.href (urlValue url) $ toHtml $ "@" <> name
    toHtml (":" :: Text)

withoutRetweetPrefix :: Text -> Text
withoutRetweetPrefix s = fromMaybe s (T.stripPrefix "RT " s)

withBreakTags :: Text -> Text
withBreakTags txt = T.intercalate "<br/>" (T.lines txt)

twitterUrl username = fromJust $ parseUrl ("https://twitter.com/" <> username)

-- | Replace status entities with anchor tags.
withUrls :: TWT.Status -> Text
withUrls s =
  let txt = statusText s
      mediaEntity (Entity _ i) = (Nothing, i)
      urlLink txt = (\url -> (renderDisplayUrl url, renderUrl url)) <$> (parseUrl txt)
      urlEntity (Entity b i) = (urlLink (TWT.ueExpanded b), i)
      mentionEntity (Entity b i) = (Just ("@" <> TWT.userEntityUserScreenName b, renderUrl (twitterUrl (TWT.userEntityUserScreenName b))), i)
      mediaEntities = mediaEntity <$> maybe [] TWT.enMedia (TWT.statusEntities s)
      urlEntities = urlEntity <$> maybe [] TWT.enURLs (TWT.statusEntities s)
      mentionEntities = mentionEntity <$> maybe [] TWT.enUserMentions (TWT.statusEntities s)
      entities = L.sortOn (\(_, idx) -> listToMaybe idx) (mediaEntities <> urlEntities <> mentionEntities)
      entityAnchor Nothing = ""
      entityAnchor (Just (label, url)) = "<a href=\"" <> url <> "\">" <> label <> "</a>"
      linkify t b i ((link, (s : e : [])) : es) = linkify t (b <> (T.drop i (T.take s t)) <> entityAnchor link) e es
      linkify t b i (_ : es) = linkify t b i es
      linkify t _ 0 ([]) = t
      linkify _ b _ ([]) = b
   in withoutRetweetPrefix $ linkify txt "" 0 entities
