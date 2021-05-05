{-# LANGUAGE OverloadedStrings #-}

module App.Model.Error where

import App.Model.URL
import Control.Exception
import Data.Text
import Database.Selda
import Network.HTTP.Types

type FieldName = Text

type ErrorMessage = Text

-- | The type for all application errors.
data AppError
  = InputError FieldName ErrorMessage
  | ForbiddenError ErrorMessage
  | InternalError ErrorMessage
  | ImportError URL ImportError
  deriving (Eq, Show)

instance Exception AppError where
  displayException (InputError _ msg) = unpack msg
  displayException (ForbiddenError msg) = unpack msg
  displayException (InternalError msg) = unpack msg
  displayException (ImportError _ err) = displayException err

data ImportError
  = NoFeedFound
  | NoAuthorFound
  | FormatNotSupported
  | NoResponse
  deriving (Eq, Show, Read, Bounded, Enum)

instance SqlType ImportError

instance Exception ImportError where
  displayException NoFeedFound = "No feed found at that URL."
  displayException NoAuthorFound = "No author information found at that URL."
  displayException FormatNotSupported = "Unsupported feed format."
  displayException NoResponse = "Nobody responded at that URL. Are you sure it's correct?"

toAppError :: (Exception e) => e -> AppError
toAppError e =
  case fromException (toException e) of
    Just (DbError _) -> InternalError "We're taking a short nap. Come back later."
    _ -> InternalError "Uh oh! Something went wrong on our end. Try again, or come back later."

rethrowAsAppError :: (Exception e) => e -> IO a
rethrowAsAppError = throwIO . toAppError

-- | Return HTTP status for different exception types.
errorStatus :: (Exception e) => Maybe e -> Status
errorStatus em =
  case em of
    Nothing -> status200
    Just e -> case fromException (toException e) of
      (Just (InputError _ _)) -> status400
      (Just (ForbiddenError _)) -> status403
      _ -> status500
