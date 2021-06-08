module App.View.Discover where

import App.Model.Author
import App.Model.Env
import App.Model.Feed
import App.Model.FeedDetailed
import App.Model.Image as Image
import App.View.URL
import Control.Applicative
import Control.Monad.Extra
import Text.Blaze.Html ((!), customAttribute, preEscapedToHtml, textValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

discoverPageHtml env categoryM feedsDtld = do
  H.h1 "Discover"
  discoverHtml env (appUrl env +> ["discover"]) categoryM feedsDtld

discoverHtml env discoverUrl categoryM feedsDtld = do
  case categoryM of
    Nothing -> H.h2 "Featured Feeds"
    Just category -> H.h2 $ toHtml $ "Featured " <> category
  H.nav $ H.ul $ do
    forM_ feedCategories $ \cat -> do
      H.li $ H.small $ do
        let categoryUrl = discoverUrl ?> [("category", cat)]
        H.a
          ! A.class_ "button outline"
          ! A.href (urlValue categoryUrl)
          $ toHtml cat
  forM_ feedsDtld (feedSnippetHtml env)

feedSnippetHtml env feedDtld = do
  let feed = feedInfo feedDtld
      author = feedAuthor feedDtld
  H.div ! A.class_ "h-cite h-feed" $ do
    H.div ! A.class_ "byline" $ do
      whenJust (Image.cacheUrl env 128 128 True <$> authorImageUrl author) $ \imageUrl -> do
        H.a ! A.href (urlValue (localFeedUrl feed)) ! A.class_ "image" $ do
          H.img
            ! A.class_ "u-photo"
            ! A.src (urlValue imageUrl)
            ! A.width "32"
            ! A.height "32"
            ! customAttribute "aria-hidden" "true"
            ! A.alt (textValue (feedDisplayName feedDtld))
      H.a ! A.href (urlValue (localFeedUrl feed)) ! A.class_ "p-name" $
        toHtml (feedDisplayName feedDtld)
    whenJust (feedSummary feed <|> authorNote author) $ \summary -> do
      H.p ! A.class_ "p-summary" $ preEscapedToHtml summary
