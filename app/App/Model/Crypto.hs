{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Model.Crypto where

import qualified Codec.Base16 as Hex
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), nullIV)
import Crypto.Error (eitherCryptoError)
import Crypto.Hash (Digest, SHA256, SHA3_512, hash)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Either.Combinators
import Data.Text
import qualified Data.Text.Encoding as TE

data Key c a where
  Key :: (BlockCipher c) => ByteString -> Key c ByteString

-- Trim secret to 256 bits (32 * 8).
initKey :: forall c. (BlockCipher c) => c -> Text -> Key c ByteString
initKey _ secret = Key (B.take 32 (TE.encodeUtf8 secret))

initCipher :: (BlockCipher c) => Key c ByteString -> Maybe c
initCipher (Key k) = case eitherCryptoError (cipherInit k) of
  Left _ -> Nothing
  Right c -> return c

encrypt :: Text -> Text -> Maybe Text
encrypt secret msg = do
  cipher <- initCipher (initKey (undefined :: AES256) secret)
  let cipherbs = ctrCombine cipher nullIV (TE.encodeUtf8 msg)
  return (Hex.encode cipherbs)

decrypt :: Text -> Text -> Maybe Text
decrypt secret hex = do
  cipher <- initCipher (initKey (undefined :: AES256) secret)
  ciphertext <- rightToMaybe $ Hex.decode hex
  let plainbs = ctrCombine cipher nullIV ciphertext
  return (TE.decodeUtf8 plainbs)

sha512text :: Text -> Text
sha512text t = sha512bs (TE.encodeUtf8 t)

sha512bs :: ByteString -> Text
sha512bs bs = pack (show (hash bs :: Digest SHA3_512))

hmac_sha256 :: B.ByteString -> B.ByteString -> B.ByteString
hmac_sha256 key msg =
  let keyBA = BA.convert key :: BA.Bytes
      msgBA = BA.convert msg :: BA.Bytes
      digest = hmacGetDigest (hmac keyBA msgBA :: HMAC SHA256)
   in BA.convert digest
