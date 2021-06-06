module App.Controller.Newsletter where

import App.Model.Author as Author
import App.Model.Database as DB hiding (text)
import App.Model.Entry as Entry
import App.Model.Env
import App.Model.Feed as Feed
import App.Model.Following as Following
import App.Model.Microformats2 as MF2
import App.Model.User as User
import App.View.Language
import Control.Exception (try)
import Control.Monad (when)
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe
import Data.Time.Clock
import Data.UUID.V4 as UUIDv4
import Web.Twain

postNewsletterWebhook :: RouteM AppEnv a
postNewsletterWebhook = do
  appEnv <- env
  let secret = appNewsletterWebhookSecret appEnv
  secretParam <- maybe respondError pure =<< paramMaybe "secret"
  senderEmail <-
    maybe respondError pure =<< (parseInputUrl =<<)
      <$> paramMaybe "from_email"
  senderName <- paramMaybe "from_name"
  newsletterId <- maybe respondError pure =<< paramMaybe "recipient_id"
  subject <- maybe respondError pure =<< paramMaybe "subject"
  body <- maybe respondError pure =<< paramMaybe "body"
  when (secretParam /= secret) respondError
  user <-
    maybe respondError pure
      =<< DB.exec (User.findOneByNewsletterId newsletterId)
  let baseUrl = withoutUserInfo senderEmail
  now <- liftIO getCurrentTime
  id <- liftIO UUIDv4.nextRandom
  existingAuthorM <- DB.exec $ Author.findOneByUrl baseUrl
  let newAuthor =
        (emptyAuthor baseUrl)
          { authorName = senderName,
            authorImportedAt = Just now
          }
      author = fromMaybe newAuthor existingAuthorM
      oneWeekAgo = addUTCTime (-604800) now
  updatedAuthor <-
    if (isNothing existingAuthorM || authorImportedAt author < Just oneWeekAgo)
      then do
        updatedAuthorE <- liftIO (try (MF2.importAuthor (authorUrl author)) :: IO (Either AppError Author))
        return $ fromMaybe author (rightToMaybe updatedAuthorE)
      else return author
  DB.exec $ Author.save (updatedAuthor {authorImportedAt = Just now})
  let eUrl = fromUuid id
      fUrl = fromJust (parseUrl (newsletterId <> "/" <> renderDisplayUrl senderEmail))
      feed =
        Feed
          { feedUrl = fUrl,
            feedAuthorUrl = authorUrl updatedAuthor,
            feedName = Just (renderDisplayUrl senderEmail),
            feedSummary = Nothing,
            feedFormat = EmailFeedFormat,
            feedRecentEntryUrl = Just eUrl,
            feedEtag = Nothing,
            feedUpdatedAt = Just now,
            feedImportedAt = Nothing,
            feedImportError = Nothing,
            feedCreatedAt = Nothing
          }
      content = wrapInParagraph <$> nullifyText (sanitizeAndProxyImageHtml appEnv baseUrl body)
      summary = summaryFromHtml (Just summaryLength) <$> (nullifyText =<< content)
      entry =
        Entry
          { entryUrl = eUrl,
            entryAuthorUrl = authorUrl updatedAuthor,
            entryFeedUrl = fUrl,
            entryName = Just subject,
            entrySummary = summary,
            entryContent = content,
            entryImageUrls = [],
            entryAudioUrls = [],
            entryVideoUrls = [],
            entryRebloggedBy = Nothing,
            entryRebloggedAt = Nothing,
            entryPublishedAt = Just now,
            entryUpdatedAt = Just now,
            entryImportedAt = Nothing
          }
      following =
        Following
          { followingUserId = userId user,
            followingFeedUrl = feedUrl feed,
            followingMuted = False
          }
  DB.exec $ Feed.save feed
  DB.exec $ Entry.save entry
  DB.exec $ Following.saveIfNew following
  send $ status status204 $ text ""
  where
    respondError = send $ status status400 $ text "Missing parameters"
