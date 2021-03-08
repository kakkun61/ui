{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}

module Example.Data
  ( Flags (..)
  , Model (..)
  , Message (..)
  , Command (..)
  , Subscription
  ) where

import qualified Example.Protobuf.Component             as C
import qualified Example.Protobuf.Component.Button      as CB
import qualified Example.Protobuf.Component.Component   as CC
import qualified Example.Protobuf.Component.View        as CV
import           Example.Protobuf.Flags                 (Flags (Flags))
import qualified Example.Protobuf.Message               as M
import qualified Example.Protobuf.Message.ButtonClicked as MB
import qualified Example.Protobuf.Message.Message       as MM

import           Hui      (View (Button, View, children, content, onClick))
import qualified Hui
import           Hui.Code (Decode (decode), Encode (encode))

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence        as Seq
import qualified Data.Text.Encoding   as Text
import           Foreign.C.Types      (CInt (CInt))
import qualified Text.ProtocolBuffers as PB

class ConvertIn m p | m -> p, p -> m where
  convertIn :: p -> m

class ConvertOut m p | m -> p, p -> m where
  convertOut :: m -> p

instance Decode Flags

newtype Model = Model { count :: Word } deriving (Show, Read, Eq, Ord)

data Message = ButtonClicked deriving (Show, Read, Eq, Ord)

instance Encode Message where
  encode = encode . convertOut

instance Encode M.Message

instance Decode Message where
  decode = (convertIn <$>) . decode

instance Decode M.Message

instance ConvertIn Message M.Message where
  convertIn (M.Message (Just (MM.ButtonClicked MB.ButtonClicked))) = ButtonClicked

instance ConvertOut Message M.Message where
  convertOut ButtonClicked = M.Message $ Just $ MM.ButtonClicked MB.ButtonClicked

instance Encode (View Message) where
  encode = encode . convertOut

instance Encode C.Component

instance ConvertOut (View Message) C.Component where
  convertOut View { children } = C.Component $ Just $ CC.View CV.View { CV.children = convertOut <$> children }
  convertOut Button { content, onClick } =
    C.Component
      $ Just
      $ CC.Button
          CB.Button
            { CB.content = PB.Utf8 $ BSL.fromStrict $ Text.encodeUtf8 content
            , CB.onClick = convertOut <$> onClick
            }

instance Encode [Command] where
  encode _ = BS.empty

data Command = NoOp deriving (Show, Read, Eq, Ord)

type Subscription = ()
