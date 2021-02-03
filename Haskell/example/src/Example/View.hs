{-# LANGUAGE NamedFieldPuns #-}

module Example.View () where

import Hui      (View (Button, View, children, content))
import Hui.Code (Encode (encode))

import qualified Data.Sequence                                    as Seq
import qualified Example.ProtocolBuffers.View.Component           as PB
import qualified Example.ProtocolBuffers.View.Component.Button    as PBB
import qualified Example.ProtocolBuffers.View.Component.Component as PBC
import qualified Example.ProtocolBuffers.View.Component.View      as PBV
import qualified Data.Text.Encoding as Text
import qualified Text.ProtocolBuffers as PB
import qualified Data.ByteString.Lazy as BSL

instance Encode (View message) where
  encode = encode . convert

instance Encode PB.Component

convert :: View message -> PB.Component
convert View { children } = PB.Component $ Just $ PBC.View $ PBV.View { PBV.children = convert <$> children }
convert Button { content }           = PB.Component $ Just $ PBC.Button PBB.Button { PBB.content = PB.Utf8 $ BSL.fromStrict $ Text.encodeUtf8 content }
