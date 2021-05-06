{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Model.URL where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Extra
import Data.Either.Combinators
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text as T
import Data.UUID as UUID
import Database.Selda.SqlType
import Text.Blaze.Html (AttributeValue, textValue)
import Text.URI hiding (relativeTo)
import qualified Text.URI as URI

newtype URL = URL URI deriving (Show, Ord)

instance Eq URL where
  (==) (URL a) (URL b) = (==) a b

instance SqlType URL where
  mkLit url = LCustom TText . LText $ renderUrl $ withoutEndingSlash url
  sqlType _ = TText
  fromSql (SqlString x) = URL $ fromMaybe (error $ "fromSql: uri column with non-uri value: " ++ show x) (mkURI x)
  fromSql _ = error "fromSql: unexpected type"
  defaultValue = mkLit (URL emptyURI)

instance SqlType [URL] where
  mkLit ([]) = LCustom TText (LNull :: Lit (Maybe Text))
  mkLit urls = LCustom TText $ LJust $ LText $ T.intercalate "<>" (renderUrl <$> urls)
  sqlType _ = TText
  fromSql (SqlNull) = []
  fromSql (SqlString x) = catMaybes (parseUrl <$> T.splitOn "<>" x)
  fromSql _ = error "fromSql: unexpected type"
  defaultValue = mkLit []

newtype URLException = URLException Text deriving (Eq, Show)

instance Exception URLException

parseUrl :: MonadThrow m => Text -> m URL
parseUrl txt = do
  uri@(URI _ auth _ _ _) <- mkURI txt
  let scheme = unRText <$> uriScheme uri
  when (maybe False (\a -> "https" /= a && "http" /= a) scheme)
    $ throwM
    $ URLException "Only http and https schemes are allowed."
  case auth of
    Left _ -> return $ URL uri
    Right a -> do
      let hostTxt = unRText $ authHost a
      if "." `T.isInfixOf` hostTxt || hostTxt == "localhost"
        then return $ URL uri
        else throwM $ URLException "URL authority must contain a TLD extension."

parseAbsoluteUrl :: MonadThrow m => Text -> m URL
parseAbsoluteUrl txt = do
  url@(URL uri) <- parseUrl txt
  when (isLeft (uriAuthority uri) || isNothing (uriScheme uri))
    $ throwM
    $ URLException "URL must be absolute, with a scheme and authority."
  return url

-- | Parse an absolute URL, defaulting to "https" scheme.
parseInputUrl :: MonadThrow m => Text -> m URL
parseInputUrl txt = if "http" `T.isPrefixOf` txt then parseAbsoluteUrl txt else parseAbsoluteUrl ("https://" <> txt)

fromUuid :: UUID -> URL
fromUuid uuid =
  let pathPiece = fromJust $ mkPathPiece (UUID.toText uuid)
      path = Just (False, NE.fromList [pathPiece])
   in URL $ URI Nothing (Left False) path [] Nothing

toURI :: URL -> URI
toURI (URL uri) = uri

-- | Render a URL to text.
renderUrl :: URL -> Text
renderUrl (URL uri) = render uri

-- | Render a URL for display, without scheme and ending slashes.
renderDisplayUrl :: URL -> Text
renderDisplayUrl (URL (URI _ auth path qs _)) = stripUrlSlashes $ render $ URI Nothing auth path qs Nothing
  where
    stripUrlSlashes :: Text -> Text
    stripUrlSlashes url =
      let t0 = fromMaybe url $ stripPrefix "//" url
          t1 = fromMaybe t0 $ stripPrefix "/" t0
       in fromMaybe t1 $ stripSuffix "/" t1

renderAuthority :: URL -> Maybe Text
renderAuthority (URL (URI _ (Right a) _ _ _)) = Just (unRText (authHost a) <> maybe "" ((":" <>) . T.pack . show) (authPort a))
renderAuthority _ = Nothing

renderPath :: URL -> Text
renderPath (URL (URI _ (Left False) pM _ _)) =
  T.intercalate "/" (unRText <$> maybe [] (NE.toList . snd) pM)
renderPath (URL (URI _ _ pM _ _)) =
  "/" <> T.intercalate "/" (unRText <$> maybe [] (NE.toList . snd) pM)

-- | Return blaze-html attribute value from URL.
urlValue :: URL -> AttributeValue
urlValue = textValue . renderUrl

isRelative :: URL -> Bool
isRelative (URL (URI _ auth _ _ _)) = isLeft auth

urlScheme :: URL -> Maybe Text
urlScheme (URL uri) = unRText <$> uriScheme uri

urlAuthority :: URL -> Maybe Authority
urlAuthority (URL uri) = either (const Nothing) Just (uriAuthority uri)

urlPort :: URL -> Maybe Int
urlPort (URL uri) = fromInteger . toInteger <$> (either (const Nothing) authPort $ uriAuthority uri)

withHttp :: URL -> URL
withHttp url@(URL (URI _ auth path query frag)) =
  if isRight auth then URL $ URI (mkScheme "http") auth path query frag else url

withHttps :: URL -> URL
withHttps url@(URL (URI _ auth path query frag)) =
  if isRight auth then URL $ URI (mkScheme "https") auth path query frag else url

withoutScheme :: URL -> URL
withoutScheme (URL (URI _ auth path query frag)) = URL $ URI Nothing auth path query frag

withoutUserInfo :: URL -> URL
withoutUserInfo (URL (URI scheme (Right a) path query frag)) =
  URL $ URI scheme (Right (Authority Nothing (authHost a) Nothing)) path query frag
withoutUserInfo url = url

withEndingSlash :: URL -> URL
withEndingSlash (URL (URI scheme auth (Just (False, path)) query frag)) =
  URL (URI scheme auth (Just (True, path)) query frag)
withEndingSlash url = url

matchScheme :: URL -> URL -> URL
matchScheme (URL (URI s _ _ _ _)) (URL (URI _ a p q f)) = URL $ URI s a p q f

withoutEndingSlash :: URL -> URL
withoutEndingSlash (URL (URI scheme auth (Just (True, path)) query frag)) =
  URL (URI scheme auth (Just (False, path)) query frag)
withoutEndingSlash url = url

withoutPath :: URL -> URL
withoutPath (URL (URI scheme auth _ _ _)) = URL $ URI scheme auth Nothing [] Nothing

-- Append path pieces to URL.
appendPath :: URL -> [Text] -> URL
appendPath url@(URL (URI scheme auth path query frag)) ps =
  case nonEmpty (catMaybes (mkPathPiece <$> ps)) of
    Nothing -> url
    Just rps ->
      case path of
        Nothing -> URL $ URI scheme auth (Just (False, rps)) query frag
        Just (_, bps) -> URL $ URI scheme auth (Just (False, bps <> rps)) query frag

(+>) = appendPath

appendQuery :: URL -> [(Text, Text)] -> URL
appendQuery (URL (URI scheme auth path query frag)) qs =
  let mkQueryParam (k, v) = do
        rk <- mkQueryKey k
        rv <- mkQueryValue v
        return (QueryParam rk rv)
      nqs = catMaybes (mkQueryParam <$> qs)
   in URL $ URI scheme auth path (query <> nqs) frag

(?>) = appendQuery

mapFragment :: URL -> Text -> URL
mapFragment (URL (URI scheme auth path query _)) frag =
  URL $ URI scheme auth path query (mkFragment frag)

(#>) = mapFragment

-- | Resolve URL relative to base URL. If base URL is not absolute, the reference URL is returned.
relativeTo :: URL -> URL -> URL
relativeTo (URL (URI _ (Left False) p q f)) (URL (URI _ (Left True) Nothing _ _)) =
  URL (URI Nothing (Left True) p q f)
relativeTo url@(URL ref) (URL base) = fromMaybe url (URL <$> URI.relativeTo ref base)
