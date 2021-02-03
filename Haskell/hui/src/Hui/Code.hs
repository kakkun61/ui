{-# LANGUAGE DefaultSignatures #-}

module Hui.Code
 ( Encode (..)
 , Decode (..)
 ) where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either.Result   (fromEither, toMonadFail)
import           Text.ProtocolBuffers (ReflectDescriptor, Wire, messageGet, messagePut)

class Encode a where
  encode :: a -> ByteString
  default encode :: (ReflectDescriptor a, Wire a) => a -> ByteString
  encode = BSL.toStrict . messagePut

class Decode a where
  decode :: MonadFail m => ByteString -> m a
  default decode :: (ReflectDescriptor a, Wire a, MonadFail m) => ByteString -> m a
  decode = (fst <$>) . toMonadFail . fromEither . messageGet . BSL.fromStrict
