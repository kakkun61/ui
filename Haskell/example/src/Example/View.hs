{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Example.View () where

import           Example.Data                         (Message)
import qualified Example.Message                      as Message
import qualified Example.Protobuf.Component           as C
import qualified Example.Protobuf.Component.Button    as CB
import qualified Example.Protobuf.Component.Component as CC
import qualified Example.Protobuf.Component.View      as CV

import Hui      (View (Button, View, children, content, onClick))
import Hui.Code (Encode (encode))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence        as Seq
import qualified Data.Text.Encoding   as Text
import qualified Text.ProtocolBuffers as PB

instance Encode (View Message) where
  encode = encode . convertOut

instance Encode C.Component

convertOut :: View Message -> C.Component
convertOut View { children } = C.Component $ Just $ CC.View CV.View { CV.children = convertOut <$> children }
convertOut Button { content, onClick } =
  C.Component
    $ Just
    $ CC.Button
        CB.Button
          { CB.content = PB.Utf8 $ BSL.fromStrict $ Text.encodeUtf8 content
          , CB.onClick = Message.convertOut <$> onClick
          }
