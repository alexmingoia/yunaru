{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Model.User where

import App.Model.EmailAddress
import App.Model.Selda
import Control.Monad
import Crypto.KDF.BCrypt as BCrypt
import Data.Aeson as JSON
import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Data.Text.Encoding

type UserID = UUID

data User
  = User
      { userId :: UUID,
        userEmail :: Maybe Email,
        userPassword :: Maybe Text,
        userNewsletterId :: Maybe Text,
        userPaidAt :: Maybe UTCTime,
        userStripeCustomerId :: Maybe Text,
        userCreatedAt :: UTCTime
      }
  deriving (Generic)

instance SqlRow User

instance FromJSON User where
  parseJSON (Object o) = do
    id <- o .: "userId"
    email <- o .: "userEmail"
    newsletterId <- o .:? "userNewsletterId"
    (statusM :: Maybe Text) <- o .:? "userStatus"
    paidAt <- o .:? "userPaidAt"
    let isActive = maybe False (== "active") statusM
    stripeCustomerId <- o .:? "userStripeCustomerId"
    createdAt <- o .: "userCreatedAt"
    return $
      User
        { userId = id,
          userEmail = email,
          userPassword = Nothing,
          userNewsletterId = newsletterId,
          userPaidAt = if isActive then Just createdAt else paidAt,
          userStripeCustomerId = stripeCustomerId,
          userCreatedAt = createdAt
        }
  parseJSON invalid =
    prependFailure
      "parsing User failed, "
      (typeMismatch "Object" invalid)

instance ToJSON User

users :: Table User
users = tableFieldMod "users" [] (toFieldName "user")

emptyUser :: UUID -> UTCTime -> User
emptyUser id createdAt =
  User
    { userId = id,
      userEmail = Nothing,
      userPassword = Nothing,
      userNewsletterId = Nothing,
      userPaidAt = Nothing,
      userStripeCustomerId = Nothing,
      userCreatedAt = createdAt
    }

userRegistered :: User -> Bool
userRegistered u = isJust (userPassword u) && isJust (userEmail u)

userPaid :: User -> Bool
userPaid u = isJust (userPaidAt u)

findOne :: UUID -> SeldaT PG IO (Maybe User)
findOne uid = fmap listToMaybe <$> query $ do
  u <- select users
  restrict (u ! #userId .== literal uid)
  return u

findOneByEmail :: Email -> SeldaT PG IO (Maybe User)
findOneByEmail email = fmap listToMaybe <$> query $ do
  u <- select users
  restrict
    ( u ! #userEmail .== literal (Just (lowercaseEmail email))
        .|| u ! #userEmail .== literal (Just email)
    )
  return u

findOneByNewsletterId :: Text -> SeldaT PG IO (Maybe User)
findOneByNewsletterId newsletterId = fmap listToMaybe <$> query $ do
  u <- select users
  restrict (u ! #userNewsletterId .== literal (Just newsletterId))
  return u

verifyPassword :: User -> Text -> Bool
verifyPassword u p =
  case userPassword u of
    Nothing -> False
    Just h ->
      let pbs = encodeUtf8 (toLower p)
          hbs = encodeUtf8 h
       in BCrypt.validatePassword pbs hbs

setPassword :: User -> Text -> IO User
setPassword u p = do
  h <- BCrypt.hashPassword 12 (encodeUtf8 (toLower p))
  return $ u {userPassword = Just (decodeUtf8 h)}

save :: User -> SeldaT PG IO ()
save u = transaction $ do
  updated <-
    update
      users
      (\r -> r ! #userId .== literal (userId u))
      ( \r ->
          r
            `with` [ #userEmail := literal (userEmail u),
                     #userPassword
                       := if isNothing (userPassword u)
                         then r ! #userPassword
                         else literal (userPassword u),
                     #userNewsletterId := literal (userNewsletterId u),
                     #userPaidAt := literal (userPaidAt u)
                   ]
      )
  when (updated == 0) (insert_ users [u])

delete :: User -> SeldaT PG IO ()
delete user = deleteFrom_ users (\u -> u ! #userId .== literal (userId user))
