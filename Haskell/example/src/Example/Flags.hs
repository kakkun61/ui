module Example.Flags
  ( Flags (..)
  ) where

import Example.ProtocolBuffers.Flags.Flags (Flags (Flags))

import Hui.Code (Decode)

instance Decode Flags
