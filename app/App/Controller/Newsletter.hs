module App.Controller.Newsletter where

import App.Model.Author as Author
import App.Model.Database as DB
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
import Network.Wai.Responder

postNewsletterWebhook :: Responder AppEnv IO ()
postNewsletterWebhook = do
  env <- getEnv
  secret <- maybe respondError pure =<< getParam "secret"
  senderEmail <- maybe respondError pure =<< (parseInputUrl =<<) <$> getParam "from_email"
  senderName <- getParam "from_name"
  newsletterId <- maybe respondError pure =<< getParam "recipient_id"
  subject <- maybe respondError pure =<< getParam "subject"
  body <- maybe respondError pure =<< getParam "body"
  when (secret /= appNewsletterWebhookSecret env) respondError
  user <- maybe respondError pure =<< DB.exec (User.findOneByNewsletterId newsletterId)
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
            feedUpdatedAt = Just now,
            feedImportedAt = Nothing,
            feedImportError = Nothing
          }
      content = wrapInParagraph <$> nullifyText (sanitizeAndProxyImageHtml env baseUrl body)
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
            entryImportedAt = Just now
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
  send $ plaintext status204 ""
  where
    respondError = send $ plaintext status400 "Missing parameters"
