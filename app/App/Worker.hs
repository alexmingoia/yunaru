{-# LANGUAGE OverloadedStrings #-}

module App.Worker where

import App.Controller.Error as Error
import App.Model.Database as DB
import App.Model.Email as Email
import App.Model.EntryDetailed as EntryDetailed
import App.Model.Env
import App.Model.Feed as Feed
import App.Model.FeedDetailed as FeedDetailed
import App.Model.MagicLink as MagicLink
import App.Model.RemoteFeed as RemoteFeed
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception
import Control.Monad
import Data.Text as T
import Data.Time.Clock

delay = 3000000 -- three second delay between batches

-- TODO: Import reblogged author feeds.
performJobs :: IO ()
performJobs = do
  forkIO $ importFeedsJob
  sendMagicLinksJob

-- | Import latest feed entries.
importFeedsJob :: IO ()
importFeedsJob = do
  DB.withConn $ \env -> do
    -- Import new entries from feeds.
    fds <- DB.execEnv env FeedDetailed.dequeueFeedsToImport
    forConcurrently_ fds $ \feedDtld -> do
      let feed = feedInfo feedDtld
      handle (Error.ignore env) $ handle (saveImportError env feed) $ do
        when (appDebug env) $ putStrLn $ T.unpack $
          "Importing entries at " <> renderUrl (feedUrl feed)
        (ufd, eds) <- RemoteFeed.updateFeed env feedDtld
        let uf = feedInfo ufd
        -- Update feed URL if different from previous feed URL (redirected or changed).
        when (feedUrl uf /= feedUrl feed) $ do
          DB.execEnv env (Feed.updateURL feed (feedUrl uf))
        DB.execEnv env (FeedDetailed.save ufd)
        DB.execEnv env (EntryDetailed.saveAll eds)
  threadDelay delay >> importFeedsJob

saveImportError :: AppEnv -> Feed -> AppError -> IO ()
saveImportError env feed (ImportError _ e) = do
  when (appDebug env) (putStrLn (displayException e))
  now <- getCurrentTime
  let updatedFeed = feed {feedImportError = Just e, feedImportedAt = Just now}
  DB.execEnv env $ Feed.save updatedFeed
saveImportError env _ e = when (appDebug env) (putStrLn (displayException e))

sendMagicLinksJob :: IO ()
sendMagicLinksJob = do
  DB.withConn $ \env -> do
    ms <- DB.execEnv env MagicLink.dequeueUnsentMagicLinks
    forM_ ms (handle (Error.ignore env) . Email.sendMagicLink env)
  threadDelay delay >> sendMagicLinksJob
