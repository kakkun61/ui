module ExampleSpec
  ( spec
  ) where

import Example

import Hui
import Hui.Code

import Test.Hspec

import           Control.Monad.Cont
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import           Foreign
import           Foreign.C.Types

instance Encode Flags

spec :: Spec
spec = do
  describe "main" $ do
    it "init" $ do
      let
        BS.PS flagsFPtr _ flagsSize = encode Flags
        viewSize :: Int
        viewSize = 256
      flip runContT pure $ do
        flagsPtr <- ContT $ withForeignPtr flagsFPtr
        modelPtrPtr <- ContT alloca
        viewPtr <- ContT $ allocaBytes viewSize
        writtenViewSizePtr <- ContT alloca
        givePtr <- liftIO $ cGive give
        liftIO $ main initInTag flagsPtr (fromIntegral flagsSize) modelPtrPtr viewPtr (fromIntegral viewSize) writtenViewSizePtr nullPtr 0 givePtr

    it "cmd" $ do
      let
        BS.PS messageFPtr _ messageSize = encode ButtonClicked
        viewSize :: Int
        viewSize = 256
      flip runContT pure $ do
        modelPtrPtr <- ContT alloca
        modelPtr <- liftIO $ newStablePtr $ Model 0
        liftIO $ poke modelPtrPtr modelPtr
        viewPtr <- ContT $ allocaBytes viewSize
        writtenViewSizePtr <- ContT alloca
        messagePtr <- ContT $ withForeignPtr messageFPtr
        givePtr <- liftIO $ cGive give
        liftIO $ main cmdInTag nullPtr 0 modelPtrPtr viewPtr (fromIntegral viewSize) writtenViewSizePtr messagePtr (fromIntegral messageSize) givePtr

foreign import ccall "wrapper" cGive :: Give -> IO (FunPtr Give)

give :: Give
give _outTag = pure ()
