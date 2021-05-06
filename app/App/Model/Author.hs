{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.Author where

import App.Model.Selda
import App.Model.URL
import Data.List as L
import Data.Maybe

type AuthorURL = URL

data Author
  = Author
      { authorUrl :: AuthorURL,
        authorName :: Maybe Text,
        authorImageUrl :: Maybe URL,
        authorNote :: Maybe Text,
        authorImportedAt :: Maybe UTCTime
      }
  deriving (Generic, Show)

instance SqlRow Author

authors :: Table Author
authors = tableFieldMod "authors" [] (toFieldName "author")

emptyAuthor url = Author url Nothing Nothing Nothing Nothing

findOneByUrl :: URL -> SeldaT PG IO (Maybe Author)
findOneByUrl url = fmap listToMaybe <$> query $ do
  a <- select authors
  restrict (a ! #authorUrl .== literal url)
  return a

existsByURL :: URL -> SeldaT PG IO Bool
existsByURL url = fmap (not . L.null) <$> query $ do
  as <- select authors
  restrict (as ! #authorUrl .== literal url)
  return (as ! #authorUrl)

save :: Author -> SeldaT PG IO ()
save author = transaction $ do
  let httpUrl = withHttp $ authorUrl author
      httpsUrl = withHttps $ authorUrl author
  updated <-
    update
      authors
      (\a -> a ! #authorUrl .== literal httpsUrl .|| a ! #authorUrl .== literal httpUrl)
      ( \a ->
          a
            `with` [ #authorUrl := literal (authorUrl author),
                     #authorName := literal (authorName author),
                     #authorImageUrl := literal (authorImageUrl author),
                     #authorNote := literal (authorNote author),
                     #authorImportedAt := literal (authorImportedAt author)
                   ]
      )
  if updated == 0 then insert_ authors [author] else pure ()
