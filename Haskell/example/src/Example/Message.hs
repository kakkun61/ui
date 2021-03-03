module Example.Message
  ( convertOut
  ) where

import           Example.Data                           (Command, Message (ButtonClicked))
import qualified Example.Protobuf.Message               as M
import qualified Example.Protobuf.Message.ButtonClicked as MB
import qualified Example.Protobuf.Message.Message       as MM

import Hui.Code (Decode (decode), Encode (encode))

import qualified Data.ByteString as BS

instance Encode Message where
  encode = encode . convertOut

instance Encode M.Message

instance Decode Message where
  decode = (convertIn <$>) . decode

instance Decode M.Message

convertIn :: M.Message -> Message
convertIn (M.Message (Just (MM.ButtonClicked MB.ButtonClicked))) = ButtonClicked

convertOut :: Message -> M.Message
convertOut ButtonClicked = M.Message $ Just $ MM.ButtonClicked MB.ButtonClicked

instance Encode Command where
  encode _ = BS.empty
