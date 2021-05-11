{-# LANGUAGE OverloadedStrings #-}

module App.Model.EmailAddress where

import Data.Text as T
import Database.Selda.SqlType
import Text.Blaze.Html (AttributeValue, textValue)
import Web.Twain

newtype Email = Email Text deriving (Eq, Show)

lowercaseEmail (Email e) = Email (T.toLower e)

renderEmail (Email e) = e

instance SqlType Email where
  mkLit (Email email) = LCustom TText (LText email)
  sqlType _ = TText
  fromSql (SqlString x) = Email x
  fromSql _ = error "fromSql: unexpected type"
  defaultValue = error "email has no default SQL value"

instance ParsableParam Email where
  parseParam = maybe (Left "Invalid email.") Right . parseEmail

parseEmail :: Text -> Maybe Email
parseEmail t
  | T.null (T.strip t) = Nothing
  | T.length (T.strip t) < 4 = Nothing
  | T.head (T.strip t) == '@' = Nothing
  | T.last (T.strip t) == '@' = Nothing
  | "@" `T.isInfixOf` (T.strip t) = Just (Email (T.strip t))
  | otherwise = Nothing

emailValue :: Email -> AttributeValue
emailValue e = textValue ("mailto:" <> renderEmail e)
