{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Example.Data
  ( Flags (..)
  , Model (..)
  , Message (..)
  , Command (..)
  , Subscription
  ) where

import qualified Example.Protobuf.Commands                               as C
import qualified Example.Protobuf.Commands.Command                       as CC
import qualified Example.Protobuf.Commands.Command.Command               as CCC
import qualified Example.Protobuf.Commands.Command.GetDotNetDescription  as CCG
import qualified Example.Protobuf.Commands.Command.NoOp                  as CCN
import qualified Example.Protobuf.Component                              as C
import qualified Example.Protobuf.Component.Button                       as CB
import qualified Example.Protobuf.Component.Component                    as CC
import qualified Example.Protobuf.Component.View                         as CV
import           Example.Protobuf.Flags                                  (Flags (Flags))
import qualified Example.Protobuf.Message                                as M
import qualified Example.Protobuf.Message.ButtonClicked                  as MB
import qualified Example.Protobuf.Message.DotNetDescription              as MD
import qualified Example.Protobuf.Message.DotNetDescriptionButtonClicked as MD
import qualified Example.Protobuf.Message.Message                        as MM

import Hui      (View (Button, View, children, content, onClick))
import Hui.Code (Decode (decode), Encode (encode))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence        as Seq
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as Text
import qualified Text.ProtocolBuffers as PB

class ConvertIn m p | m -> p, p -> m where
  convertIn :: p -> m

class ConvertOut m p | m -> p, p -> m where
  convertOut :: m -> p

instance Decode Flags

data Model =
  Model
    { count             :: Word
    , dotNetDescription :: Text
    }
  deriving (Show, Read, Eq, Ord)

data Message
  = ButtonClicked
  | DotNetDescriptionButtonClicked
  | DotNetDescription Text
  deriving (Show, Read, Eq, Ord)

instance Encode Message where
  encode = encode . convertOut

instance Encode M.Message

instance Decode Message where
  decode = (convertIn <$>) . decode

instance Decode M.Message

instance ConvertIn Message M.Message where
  convertIn (M.Message (Just (MM.ButtonClicked MB.ButtonClicked))) = ButtonClicked
  convertIn (M.Message (Just (MM.DotNetDescriptionButtonClicked MD.DotNetDescriptionButtonClicked))) = DotNetDescriptionButtonClicked
  convertIn (M.Message (Just (MM.DotNetDescription (MD.DotNetDescription (PB.Utf8 descriptionBsl))))) = DotNetDescription $ Text.decodeUtf8 $ BSL.toStrict descriptionBsl
  convertIn (M.Message Nothing) = error "an unexpected message"

instance ConvertOut Message M.Message where
  convertOut ButtonClicked = M.Message $ Just $ MM.ButtonClicked MB.ButtonClicked
  convertOut DotNetDescriptionButtonClicked = M.Message $ Just $ MM.DotNetDescriptionButtonClicked MD.DotNetDescriptionButtonClicked
  convertOut (DotNetDescription description) = M.Message $ Just $ MM.DotNetDescription $ MD.DotNetDescription $ PB.Utf8 $ BSL.fromStrict $ Text.encodeUtf8 description

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

data Command = NoOp | GetDotNetDescription deriving (Show, Read, Eq, Ord)

instance Encode [Command] where
  encode = encode . convertOut

instance Encode C.Commands

instance ConvertOut [Command] C.Commands where
  convertOut cs = C.Commands $ Seq.fromList $ convertOut <$> cs

instance ConvertOut Command CC.Command where
  convertOut NoOp                 = CC.Command $ Just $ CCC.NoOp CCN.NoOp
  convertOut GetDotNetDescription = CC.Command $ Just $ CCC.GetDotNetDescription CCG.GetDotNetDescription

type Subscription = ()
