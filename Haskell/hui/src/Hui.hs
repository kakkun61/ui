{-# LANGUAGE FlexibleContexts #-}

module Hui
 ( Program
 , program
 , Command
 , Message (..)
 , View (..)
 , Subscription
 , Give
 , InTag
 , initInTag
 , cmdInTag
 , OutTag
 , noopOutTag
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

data Command message = Command deriving (Show, Read, Eq, Ord)

instance Semigroup (Command message) where
  _ <> _ = Command

instance Monoid (Command message) where
  mempty = Command

data View message
  = View { children :: Seq (View message) }
  | Button { content :: Text, onClick :: Maybe message }

data Subscription message = Subscription deriving (Show, Read, Eq, Ord)

instance Semigroup (Subscription message) where
  _ <> _ = Subscription

instance Monoid (Subscription message) where
  mempty = Subscription

type Program flags model message =
  InTag -- ^ a type of an input call
  -> Ptr Word8 -- ^ a pinter to a flags memory area
  -> CInt -- ^ a size of a flags memory area in byte
  -> Ptr (StablePtr model) -- ^ a pointer to a pointer to a model
  -> Ptr Word8 -- ^ a pointer to a view memory area
  -> CInt -- ^ a size of a view memory area in byte
  -> Ptr CInt -- ^ a pointer to a size of a written view memory area
  -> Ptr Word8 -- ^ a pointer to a message memory area
  -> CInt -- ^ a size of a message memory area
  -> FunPtr Give -- ^ a pointer to a continuation
  -> IO ()

type InTag = CInt

initInTag :: InTag
initInTag = 0

cmdInTag :: InTag
cmdInTag = 1

type OutTag = CInt

noopOutTag :: OutTag
noopOutTag = 0

type Give = OutTag -> IO ()

program
  :: Message message
  => Decode flags
  => Encode (View message)
  => Decode message
  => (flags -> (model, Command message))
  -> (model -> View message)
  -> (message -> model -> (model, Command message))
  -> (model -> Subscription message)
  -> Program flags model message
program ~initialize ~view ~update ~subscriptions inTag flagsPtr cFlagsSize modelPtrPtr viewPtr cViewSize writtenViewSizePtr messagePtr cMessageSize give
  | inTag == initInTag = do
      let
        flagsSize = fromIntegral cFlagsSize :: Int
        viewSize = fromIntegral cViewSize :: Int
      flagsFPtr <- newForeignPtr_ flagsPtr
      flags <- decode $ BS.PS flagsFPtr 0 flagsSize
      let
        (model, command) = initialize flags
        outTag = messageTag command
      modelPtr <- newStablePtr model
      poke modelPtrPtr modelPtr
      hsGive give outTag
      let BS.PS viewFPtr _ writtenViewSize = encode $ view model
      when (viewSize < writtenViewSize) $ fail "an encoded view is too large"
      withForeignPtr viewFPtr $ \srcPtr -> copyBytes viewPtr srcPtr writtenViewSize
      poke writtenViewSizePtr (fromIntegral writtenViewSize)
  | inTag == cmdInTag = do
      let
        viewSize = fromIntegral cViewSize :: Int
        messageSize = fromIntegral cMessageSize :: Int
      modelPtr <- peek modelPtrPtr
      model <- deRefStablePtr modelPtr
      freeStablePtr modelPtr
      messageFPtr <- newForeignPtr_ messagePtr
      message <- decode $ BS.PS messageFPtr 0 messageSize
      let
        (newModel, command) = update message model
        outTag = messageTag command
      newModelPtr <- newStablePtr newModel
      poke modelPtrPtr newModelPtr
      hsGive give outTag
      let BS.PS viewFPtr _ writtenViewSize = encode $ view model
      when (viewSize < writtenViewSize) $ fail "an encoded view is too large"
      withForeignPtr viewFPtr $ \srcPtr -> copyBytes viewPtr srcPtr writtenViewSize
      poke writtenViewSizePtr (fromIntegral writtenViewSize)
  | otherwise = error $ "program: unknown inTag: " ++ show inTag

type CView = CInt

foreign import ccall "dynamic" hsGive :: FunPtr Give -> Give

class Message a where
  messageTag :: Command a -> OutTag
