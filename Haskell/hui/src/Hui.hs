{-# LANGUAGE FlexibleContexts #-}

module Hui
 ( Program
 , program
 , View (..)
 , Subscription
 ) where

import Hui.Code (Decode (decode), Encode (encode))

import           Control.Monad                 (when)
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Sequence                 (Seq)
import           Data.Text                     (Text)
import           Data.Word                     (Word8)
import           Foreign                       (StablePtr, Storable (peek, poke), copyBytes, deRefStablePtr,
                                                freeStablePtr, newForeignPtr_, newStablePtr, withForeignPtr)
import           Foreign.C.Types               (CInt (CInt))
import           Foreign.Ptr                   (FunPtr, Ptr)

data View message
  = View { children :: Seq (View message) }
  | Button { content :: Text, onClick :: Maybe message }

data Subscription message = Subscription deriving (Show, Read, Eq, Ord)

instance Semigroup (Subscription message) where
  _ <> _ = Subscription

instance Monoid (Subscription message) where
  mempty = Subscription

type Program flags model message command subscription =
  Ptr Word8 -- ^ a pointer to a flags memory area (input)
  -> CInt -- ^ a size of a flags memory area in byte
  -> Ptr (StablePtr model) -- ^ a pointer to a pointer to a model (input/output)
  -> Ptr Word8 -- ^ a pointer to a view memory area (output)
  -> CInt -- ^ a size of a view memory area in byte
  -> Ptr CInt -- ^ a pointer to a size of a written view memory area (output)
  -> Ptr Word8 -- ^ a pointer to a message memory area (input)
  -> CInt -- ^ a size of a message memory area
  -> Ptr Word8 -- ^ a pointer to a command memory area (output)
  -> CInt -- ^ a size of a command memory area
  -> Ptr CInt -- ^ a pointer to a size of a written command memory area (output)
  -> IO ()

program
  :: ( Decode flags
     , Encode (View message)
     , Decode message
     , Encode [command]
     )
  => (flags -> (model, [command])) -- ^ initialize
  -> (model -> View message) -- ^ view
  -> (message -> model -> (model, [command])) -- ^ update
  -> (model -> [subscription]) -- ^ subscription
  -> Program flags model message command subscription
-- update
program ~initialize ~view ~update ~subscriptions _ (-1) modelPtrPtr viewPtr cViewSize writtenViewSizePtr messagePtr cMessageSize commandPtr cCommandSize writtenCommandSizePtr = do
  let
    viewSize = fromIntegral cViewSize :: Int
    messageSize = fromIntegral cMessageSize :: Int
    commandSize = fromIntegral cCommandSize :: Int
  modelPtr <- peek modelPtrPtr
  model <- deRefStablePtr modelPtr
  freeStablePtr modelPtr
  messageFPtr <- newForeignPtr_ messagePtr
  message <- decode $ BS.PS messageFPtr 0 messageSize
  let (newModel, command) = update message model
  newModelPtr <- newStablePtr newModel
  poke modelPtrPtr newModelPtr
  let BS.PS viewFPtr _ writtenViewSize = encode $ view model
  when (viewSize < writtenViewSize) $ fail "an encoded view is too large"
  withForeignPtr viewFPtr $ \srcPtr -> copyBytes viewPtr srcPtr writtenViewSize
  poke writtenViewSizePtr (fromIntegral writtenViewSize)
  let BS.PS commandFPtr _ writtenCommandSize = encode command
  when (commandSize < writtenCommandSize) $ fail "an encoded command is too large"
  withForeignPtr commandFPtr $ \srcPtr -> copyBytes commandPtr srcPtr writtenCommandSize
  poke writtenCommandSizePtr (fromIntegral writtenCommandSize)
-- initialize
program ~initialize ~view ~update ~subscriptions flagsPtr cFlagsSize modelPtrPtr viewPtr cViewSize writtenViewSizePtr messagePtr cMessageSize commandPtr cCommandSize writtenCommandSizePtr = do
  let
    flagsSize = fromIntegral cFlagsSize :: Int
    viewSize = fromIntegral cViewSize :: Int
    commandSize = fromIntegral cCommandSize :: Int
  flagsFPtr <- newForeignPtr_ flagsPtr
  flags <- decode $ BS.PS flagsFPtr 0 flagsSize
  let (model, command) = initialize flags
  modelPtr <- newStablePtr model
  poke modelPtrPtr modelPtr
  let BS.PS viewFPtr _ writtenViewSize = encode $ view model
  when (viewSize < writtenViewSize) $ fail "an encoded view is too large"
  withForeignPtr viewFPtr $ \srcPtr -> copyBytes viewPtr srcPtr writtenViewSize
  poke writtenViewSizePtr (fromIntegral writtenViewSize)
  let BS.PS commandFPtr _ writtenCommandSize = encode command
  when (commandSize < writtenCommandSize) $ fail "an encoded command is too large"
  withForeignPtr commandFPtr $ \srcPtr -> copyBytes commandPtr srcPtr writtenCommandSize
  poke writtenCommandSizePtr (fromIntegral writtenCommandSize)
